package wavebpf

import spinal.core._
import spinal.lib._

case class InsnBufferConfig(
    addrWidth: Int
)

case class InsnBufferReadContext() extends Bundle {
  val flush = Bool()
}

case class InsnBufferRefillReq(c: InsnBufferConfig) extends Bundle {
  val addr = UInt(c.addrWidth bits)
  val insn = Bits(64 bits)
}

case class InsnBufferReadReq(c: InsnBufferConfig) extends Bundle {
  val addr = UInt(c.addrWidth bits)
  val ctx = InsnBufferReadContext()
}

case class InsnBufferReadRsp(c: InsnBufferConfig) extends Bundle {
  val addr = UInt(c.addrWidth bits)
  val insn = Bits(64 bits)
  val ctx = InsnBufferReadContext()
}

case class InsnBuffer(c: InsnBufferConfig) extends Component {
  val mem = Mem(Bits(64 bits), scala.math.pow(2, c.addrWidth).toInt)
  mem.initialContent = (for(i <- 0 until mem.wordCount) yield BigInt("4242424242424242", 16)).toArray

  val io = new Bundle {
    val refill = slave Flow (InsnBufferRefillReq(c))
    val readReq = slave Stream (InsnBufferReadReq(c))
    val readRsp = master Stream (InsnBufferReadRsp(c))
  }

  val (readReqToBeStaged, readReqNow) = StreamFork2(io.readReq)
  val readReqStaged = readReqToBeStaged.stage()
  val readData =
    mem.streamReadSync(readReqNow.translateWith(readReqNow.payload.addr))

  val rsp = InsnBufferReadRsp(c)
  rsp.addr := readReqStaged.payload.addr
  rsp.insn := readData.payload
  rsp.ctx := readReqStaged.payload.ctx
  io.readRsp << StreamJoin.arg(readReqStaged, readData).translateWith(rsp)

  mem.write(
    enable = io.refill.valid,
    address = io.refill.payload.addr,
    data = io.refill.payload.insn
  )
}
