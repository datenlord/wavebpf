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

  def toAxi4WriteOnly(): Axi4WriteOnly = {
    val area = new Area {
      val axiMaster = Axi4WriteOnly(DataMemV2Axi4PortConfig())
      val axi = Axi4WriteOnly(DataMemV2Axi4PortConfig())
      axi.aw << axiMaster.aw.s2mPipe()
      axi.w << axiMaster.w.s2mPipe()
      axi.b >> axiMaster.b
      axi.setIdle()
      val awSnapshot = Reg(Axi4Aw(MMIOBusConfigV2()))

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
              awSnapshot.addr := awSnapshot.addr + WbpfUtil.decodeAxSize(
                awSnapshot.size
              )
              when(axi.w.last) {
                goto(sendWriteRsp)
              }
            }
          }
        }
        val sendWriteRsp: State = new State {
          whenIsActive {
            axi.b.valid := True
            when(axi.b.ready) {
              goto(waitForAw)
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
  private def computeMask(
      addr: UInt,
      w: SpinalEnumCraft[MemoryAccessWidth.type]
  ): Bits = {
    val aligned = MemoryAccessWidth.alignedMask(w)
    val misalignment = addr(2 downto 0)
    (aligned << misalignment).resize(aligned.getBitsWidth)
  }
  val io = new Bundle {
    val req = slave(Stream(DataMemRequest()))
    val rsp = master(Stream(DataMemResponse()))
  }
  val memBody = Mem(Bits(64 bits), c.numWords)
  memBody.initialContent =
    Array.fill(memBody.wordCount)(BigInt("4343434343434343", 16))

  val wordAddr = io.req.addr >> 3
  memBody.write(
    address = wordAddr.resized,
    data = io.req.data,
    enable = io.req.valid && io.req.write,
    mask = io.req.precomputedStrbValid.mux(
      (False, computeMask(io.req.addr, io.req.width)),
      (True, io.req.precomputedStrb)
    )
  )

  val stagedReq = io.req.stage()

  val rspData = new DataMemResponse()
  rspData.data := memBody.readSync(address = wordAddr.resized)
  rspData.ctx := stagedReq.ctx

  stagedReq.translateWith(rspData) >> io.rsp
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
    DataMemV2Port.arbitrate(users, dmPort)
  }
}

case class DataMemConfig(
    numWords: Int
)

object DataMemV2Axi4PortConfig {
  def apply() = Axi4Config(
    addressWidth = 64,
    dataWidth = 64,
    idWidth = 4
  )
}