package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.bus.wishbone._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class DataMemRequest() extends Bundle {
  val write = Bool()
  val addr = UInt(64 bits)
  val data = Bits(64 bits)
  val ctx = UInt(4 bits)
  val width = MemoryAccessWidth()
  val precomputedStrbValid = Bool()
  val precomputedStrb = Bits(8 bits)
}

case class DataMemResponse() extends Bundle {
  val data = Bits(64 bits)
  val ctx = UInt(4 bits)
}

case class DataMemV2Port() extends Bundle with IMasterSlave {
  val request = Stream(DataMemRequest())
  val response = Stream(DataMemResponse())

  override def asMaster(): Unit = {
    master(request)
    slave(response)
  }

  def toAxi4ReadOnly(): Axi4ReadOnly = {
    val area = new Area {
      val axiMaster = Axi4ReadOnly(DataMemV2Axi4PortConfig())
      var axi = WbpfUtil.axi4Pipe(axiMaster)
      axi.setBlocked()
      request.setIdle()
      response.freeRun()

      val arSnapshot = Reg(Axi4Ar(DataMemV2Axi4PortConfig()))
      val issueRemaining = Reg(UInt(arSnapshot.len.getBitsWidth bits))
      val shouldIssue = Reg(Bool())

      def issueReq() = {
        request.valid := shouldIssue

        // Signal that we do not want the DM unit to shift the data for us
        request.payload.precomputedStrbValid := True

        request.payload.addr := (U(
          0,
          32 bits
        ).asBits ## arSnapshot.addr.asBits).asUInt
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
      val axiMaster = Axi4WriteOnly(DataMemV2Axi4PortConfig())
      val axi = WbpfUtil.axi4Pipe(axiMaster)
      axi.setBlocked()
      val awSnapshot = Reg(Axi4Aw(DataMemV2Axi4PortConfig()))

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
            request.payload.addr := (U(
              0,
              32 bits
            ).asBits ## awSnapshot.addr.asBits).asUInt
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

object DataMemV2Port {
  def arbitrate(inputs: Seq[DataMemV2Port], output: DataMemV2Port) {
    assert(log2Up(inputs.length) <= DataMemRequest().ctx.getBitsWidth)

    val annotatedReq = inputs.zipWithIndex.map(x => {
      val payload = DataMemRequest()
      payload.ctx := U(x._2)
      payload.assignUnassignedByName(x._1.request.payload)
      x._1.request.translateWith(payload)
    })
    val arbStream = new StreamArbiterFactory().roundRobin.on(annotatedReq)
    output.request << arbStream
    val rspVec = Vec(inputs.map(_.response))
    val demux = StreamDemux(
      output.response,
      output.response.payload.ctx.resize(log2Up(inputs.length) bits),
      rspVec.length
    )
    rspVec.zip(demux).foreach(x => x._1 << x._2)
  }
}

case class DataMemV2Core(c: DataMemConfig) extends Component {
  case class ReqControl() extends Bundle {
    val misalignment = UInt(3 bits)
    val alignedMask = Bits(8 bits)
    val mask = Bits(8 bits)

    def expandedAlignedMask =
      alignedMask.asBools.flatMap(x => Seq.fill(8)(x)).asBits()
  }

  object ReqControl {
    def compute(req: DataMemRequest): ReqControl = {
      val ret = ReqControl()
      ret.misalignment := req.addr(2 downto 0)
      val aligned = MemoryAccessWidth.alignedMask(req.width)
      ret.alignedMask := aligned
      ret.mask := (aligned << ret.misalignment).resize(aligned.getBitsWidth)
      ret
    }
  }

  val io = new Bundle {
    val req = slave(Stream(DataMemRequest()))
    val rsp = master(Stream(DataMemResponse()))
  }
  val reqControl = ReqControl.compute(io.req.payload)
  val memBody = Mem(Bits(64 bits), c.numWords)

  val wordAddr = io.req.addr >> 3
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
    ioReqToRead.translateWith(wordAddr.resize(log2Up(c.numWords)))
  )
  val stagedReqControl = ReqControl.compute(stagedReq.payload)

  val rspData = new DataMemResponse()
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

case class DataMemV2(c: DataMemConfig) extends Area {
  val users = ArrayBuffer[DataMemV2Port]()
  val dmPort = DataMemV2Port()
  val impl = DataMemV2Core(c)
  dmPort.request >> impl.io.req
  impl.io.rsp >> dmPort.response

  def use(): DataMemV2Port = {
    val port = DataMemV2Port()
    users += port
    port
  }

  Component.current.afterElaboration {
    println("DM has " + users.length + " users")
    DataMemV2Port.arbitrate(users, dmPort)
  }
}

case class DataMemConfig(
    numWords: Int
)

object DataMemV2Axi4PortConfig {
  def apply() = Axi4Config(
    addressWidth = 32,
    dataWidth = 64,
    idWidth = 16
  )
}

object DataMemV2Axi4DownsizedPortConfig {
  def apply() = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 16
  )
}
