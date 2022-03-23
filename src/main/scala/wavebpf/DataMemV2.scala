package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.bus.wishbone._
import spinal.lib.bus.amba4.axi._

case class DataMemRequest() extends Bundle {
  val write = Bool()
  val addr = UInt(64 bits)
  val data = Bits(64 bits)
  val ctx = UInt(4 bits)
  val width = MemoryAccessWidth()
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
    val demux = StreamDemux(output.response, output.response.payload.ctx.resize(log2Up(inputs.length) bits), rspVec.length)
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
    aligned >> misalignment
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
    mask = computeMask(io.req.addr, io.req.width)
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
