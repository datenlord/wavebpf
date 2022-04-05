package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.bus.wishbone._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._
import WbpfExt._

case class BankedMemRequest() extends Bundle {
  val write = Bool()
  val addr = UInt(32 bits)
  val data = Bits(64 bits)
  val ctx = UInt(4 bits)
  val width = MemoryAccessWidth()
  val precomputedStrbValid = Bool()
  val precomputedStrb = Bits(8 bits)
}

case class BankedMemResponse() extends Bundle {
  val data = Bits(64 bits)
  val ctx = UInt(4 bits)
}

case class BankedMemPort() extends Bundle with IMasterSlave {
  val request = Stream(BankedMemRequest())
  val response = Stream(BankedMemResponse())

  override def asMaster(): Unit = {
    master(request)
    slave(response)
  }

  def toAxi4ReadOnly(): Axi4ReadOnly = {
    val area = new Area {
      val axiMaster = Axi4ReadOnly(BankedMemAxi4PortConfig())
      var axi = WbpfUtil.axi4Pipe(axiMaster)
      axi.setBlocked()
      request.setIdle()
      response.freeRun()

      val arSnapshot = Reg(Axi4Ar(BankedMemAxi4PortConfig()))
      val issueRemaining = Reg(UInt(arSnapshot.len.getBitsWidth bits))
      val shouldIssue = Reg(Bool())

      def issueReq() = {
        request.valid := shouldIssue

        // Signal that we do not want the DM unit to shift the data for us
        request.payload.precomputedStrbValid := True

        request.payload.addr := arSnapshot.addr
        request.payload.write := False
        request.payload.width := WbpfUtil.axSizeToMemAccessWidth64bit(
          arSnapshot.size
        )
        val nextAddr = (arSnapshot.addr + WbpfUtil.decodeAxSize(
          arSnapshot.size
        ))
        when(request.fire) {
          /*report(
            Seq(
              "AXI R fire addr=",
              arSnapshot.addr,
              " w=",
              request.payload.width
            )
          )*/
          arSnapshot.addr := nextAddr
          issueRemaining := issueRemaining - 1
          when(issueRemaining === 0) {
            shouldIssue := False
          }
        }
      }

      val readFsm = new StateMachine {
        val waitForAr: State = new State with EntryPoint {
          whenIsActive {
            when(axi.ar.valid) {
              axi.ar.ready := True
              arSnapshot := axi.ar.payload
              issueRemaining := axi.ar.payload.len
              shouldIssue := True
              goto(sendReadResponse)
            }
          }
        }

        val sendReadResponse: State = new State {
          whenIsActive {
            issueReq()
            axi.r.valid := response.valid
            axi.r.payload.id := arSnapshot.id
            axi.r.payload.resp := 0 // OKAY
            axi.r.payload.data := response.payload.data
            axi.r.payload.last := arSnapshot.len === 0
            response.ready := axi.r.ready

            when(axi.r.fire) {
              /*report(
                Seq(
                  "AXI R complete data=",
                  response.payload.data,
                  " last=",
                  axi.r.payload.last
                )
              )*/
              arSnapshot.len := arSnapshot.len - 1
              when(axi.r.payload.last) {
                goto(waitForAr)
              }
            }
          }
        }
      }
    }
    area.axiMaster
  }

  def toAxi4WriteOnly(): Axi4WriteOnly = {
    val area = new Area {
      val axiMaster = Axi4WriteOnly(BankedMemAxi4PortConfig())
      val axi = WbpfUtil.axi4Pipe(axiMaster)
      axi.setBlocked()
      val awSnapshot = Reg(Axi4Aw(BankedMemAxi4PortConfig()))

      request.setIdle()
      response.freeRun()

      val reqCounter = Reg(UInt(8 bits))
      val ackCounter = Reg(UInt(8 bits))

      when(response.fire) {
        ackCounter := ackCounter + 1
      }

      val writeFsm = new StateMachine {
        val waitForAw: State = new State with EntryPoint {
          whenIsActive {
            when(axi.aw.valid) {
              axi.aw.ready := True
              awSnapshot := axi.aw.payload
              goto(waitForW)
            }
          }
        }
        val waitForW: State = new State {
          onEntry {
            reqCounter := 0
            ackCounter := 0
          }
          whenIsActive {
            request.valid := axi.w.valid
            request.payload.precomputedStrbValid := True
            request.payload.precomputedStrb := axi.w.strb
            request.payload.ctx.assignDontCare()
            request.payload.addr := awSnapshot.addr
            request.payload.write := True
            request.payload.width.assignDontCare()
            request.payload.data := axi.w.payload.data
            axi.w.ready := request.ready

            when(axi.w.fire) {
              // report(Seq("AXI W fire ", awSnapshot.addr, " ", axi.w.payload.data, " ", axi.w.strb))
              awSnapshot.addr := awSnapshot.addr + WbpfUtil.decodeAxSize(
                awSnapshot.size
              )
              reqCounter := reqCounter + 1
              when(axi.w.last) {
                goto(sendWriteRsp)
              }
            }
          }
        }
        val sendWriteRsp: State = new State {
          whenIsActive {
            when(reqCounter === ackCounter) {
              axi.b.valid := True
              axi.b.payload.resp := 0 // OKAY
              axi.b.payload.id := awSnapshot.id
              when(axi.b.ready) {
                goto(waitForAw)
              }
            }
          }
        }
      }

    }
    area.axiMaster
  }
}

object BankedMemPort {
  def arbitrate(
      c: BankedMemConfig,
      inputs: Seq[BankedMemPort],
      outputs: Seq[BankedMemPort]
  ) {
    assert(log2Up(inputs.length) <= BankedMemRequest().ctx.getBitsWidth)

    val annotatedReqs = inputs.zipWithIndex.map(x => {
      val payload = BankedMemRequest()
      payload.ctx := U(x._2)
      payload.assignUnassignedByName(x._1.request.payload)
      x._1.request.translateWith(payload).setBlocked()
    })
    for ((output, bankIndex) <- outputs.zipWithIndex) {
      val bankReqs = annotatedReqs.map(x => {
        val s = Stream(x.payloadType)
        when(
          Bool(c.numBanks == 1) || U(
            bankIndex,
            log2Up(c.numBanks) bits
          ) === (x.payload.addr >> 3).resized
        ) {
          s << x
        } otherwise {
          s.setIdle()
        }
        s
      })
      val arbStream = new StreamArbiterFactory().roundRobin.on(bankReqs)
      output.request << arbStream.check(payloadInvariance = true)
    }

    // Re-ordering.
    /*val cycles = Reg(UInt(64 bits)) init(0)
    cycles := cycles + 1*/
    val robs =
      if (c.numBanks == 1) null
      else
        (0 until inputs.length).map(_ =>
          new StreamFifoLowLatency(
            dataType = UInt(log2Up(c.numBanks) bits),
            depth = c.numBanks,
            latency = 1
          )
        )
    if (robs != null) {
      for (rob <- robs) {
        rob.io.push.setIdle()
        rob.io.pop.setBlocked()
        assert(
          !rob.io.push.valid || rob.io.push.ready,
          "BankedMemPort: the ROB FIFO is full"
        )
      }
      for ((output, outputIndex) <- outputs.zipWithIndex) {
        for ((rob, robIndex) <- robs.zipWithIndex) {
          when(output.request.fire) {
            val targetRobIndex = output.request.payload.ctx
            when(targetRobIndex === robIndex) {
              rob.io.push.valid := True
              rob.io.push.payload := outputIndex
              // report(Seq("ROB push output=" + outputIndex + " input=", targetRobIndex, "cycles=", cycles))
            }
          }
        }
      }
    }
    val rspVec = Vec(inputs.map(_.response))
    rspVec.foreach(x => x.setIdle())
    for ((output, outputIndex) <- outputs.zipWithIndex) {
      output.response.setBlocked()
      for ((inputRsp, inputIndex) <- rspVec.zipWithIndex) {
        val outputMatchesInput =
          output.response.valid && output.response.payload.ctx === inputIndex
        if (robs != null) {
          val rob = robs(inputIndex)
          when(outputMatchesInput) {
            // report(Seq("ROB pop cycles=", cycles))
            assert(
              rob.io.pop.valid,
              "BankedMemPort: the ROB FIFO is empty - rob index " + inputIndex
            )
            when(rob.io.pop.valid && rob.io.pop.payload === outputIndex) {
              inputRsp << output.response
              rob.io.pop.ready := output.response.ready
            }
          }
        } else {
          when(outputMatchesInput) {
            inputRsp << output.response
          }
        }
      }
    }
  }
}

case class BankedMemCore(c: BankedMemConfig) extends Component {
  case class ReqControl() extends Bundle {
    val misalignment = UInt(3 bits)
    val alignedMask = Bits(8 bits)
    val mask = Bits(8 bits)

    def expandedAlignedMask =
      alignedMask.asBools.flatMap(x => Seq.fill(8)(x)).asBits()
  }

  object ReqControl {
    def compute(req: BankedMemRequest): ReqControl = {
      val ret = ReqControl()
      ret.misalignment := req.addr(2 downto 0)
      val aligned = MemoryAccessWidth.alignedMask(req.width)
      ret.alignedMask := aligned
      ret.mask := (aligned << ret.misalignment).resize(aligned.getBitsWidth)
      ret
    }
  }

  val io = new Bundle {
    val req = slave(Stream(BankedMemRequest()))
    val rsp = master(Stream(BankedMemResponse()))
  }
  val reqControl = ReqControl.compute(io.req.payload)
  val memBody = Mem(Bits(64 bits), c.numWordsPerBank)

  val wordAddr = (io.req.addr >> 3) >> log2Up(c.numBanks)
  memBody.write(
    address = wordAddr.resized,
    data = io.req.precomputedStrbValid.mux(
      (
        False,
        (io.req.data << (reqControl.misalignment << 3))
          .resize(io.req.data.getBitsWidth)
      ),
      (True, io.req.data)
    ),
    enable = io.req.valid && io.req.write,
    mask = io.req.precomputedStrbValid.mux(
      (False, reqControl.mask),
      (True, io.req.precomputedStrb)
    )
  )

  val (ioReqToStage, ioReqToRead) = StreamFork2(io.req)

  val stagedReq = ioReqToStage.stage()
  val readRsp = memBody.streamReadSync(
    ioReqToRead.translateWith(wordAddr.resize(log2Up(c.numWordsPerBank)))
  )
  val stagedReqControl = ReqControl.compute(stagedReq.payload)

  val rspData = new BankedMemResponse()
  rspData.data := stagedReq.precomputedStrbValid.mux(
    (
      False,
      (readRsp.payload >> (stagedReqControl.misalignment << 3)).resize(
        readRsp.payload.getBitsWidth
      ) & stagedReqControl.expandedAlignedMask
    ),
    (True, readRsp.payload)
  )
  rspData.ctx := stagedReq.ctx

  StreamJoin(stagedReq, readRsp).translateWith(rspData) >> io.rsp
  /*
  when(io.rsp.fire) {
    report(
      Seq(
        "Memory operation completed. addr=",
        stagedReq.payload.addr,
        " write=",
        stagedReq.payload.write,
        " writeData=",
        stagedReq.payload.data,
        " precomputedStrbValid=",
        stagedReq.payload.precomputedStrbValid,
        " precomputedStrb=",
        stagedReq.payload.precomputedStrb,
        " width=",
        stagedReq.payload.width,
        " readData=",
        rspData.data,
        " mask=",
        stagedReqControl.mask,
        " eam=",
        stagedReqControl.expandedAlignedMask
      )
    )
  }*/
}

case class BankedMem(c: BankedMemConfig) extends Area {
  val users = ArrayBuffer[BankedMemPort]()
  val dmPorts = (0 until c.numBanks).map(_ => BankedMemPort())
  val impls = (0 until c.numBanks).map(_ => BankedMemCore(c))
  for ((dmPort, impl) <- dmPorts.zip(impls)) {
    dmPort.request >> impl.io.req
    impl.io.rsp >> dmPort.response
  }

  def use(): BankedMemPort = {
    val port = BankedMemPort()
    users += port
    port
  }

  Component.current.afterElaboration {
    println("Banked mem " + c.name + " has " + users.length + " user(s)")
    BankedMemPort.arbitrate(c, users, dmPorts)
  }
}

case class BankedMemConfig(
    numWords: Int,
    name: String,
    numBanks: Int
) {
  assert(isPow2(numBanks))
  assert(numWords > 0 && numBanks > 0 && numWords % numBanks == 0)

  val bankAddrWidth = log2Up(numBanks)
  val numWordsPerBank = numWords / numBanks
}

object BankedMemAxi4PortConfig {
  def apply() = Axi4Config(
    addressWidth = 32,
    dataWidth = 64,
    idWidth = 16
  )
}
