package wavebpf

import spinal.core._
import spinal.lib._

case class InsnBufferConfig(
    addrWidth: Int,
    btbSize: Int
)

case class InsnBufferReadContext(c: InsnBufferConfig) extends Bundle {
  val flush = Bool()
  val flushReason = PcFlushReasonCode()
  val prediction = BranchPrediction(c)
}

case class BranchPrediction(c: InsnBufferConfig) extends Bundle {
  val valid = Bool()
  val predictedTarget = UInt(c.addrWidth bits)
}

case class InsnBufferRefillReq(c: InsnBufferConfig) extends Bundle {
  val addr = UInt(c.addrWidth bits)
  val insn = Bits(64 bits)
}

case class InsnBufferReadReq(c: InsnBufferConfig) extends Bundle {
  val addr = UInt(c.addrWidth bits)
  val ctx = InsnBufferReadContext(c)
}

case class InsnBufferReadRsp(c: InsnBufferConfig) extends Bundle {
  val addr = UInt(c.addrWidth bits)
  val insn = Bits(64 bits)
  val ctx = InsnBufferReadContext(c)

  def imm = this.insn(63 downto 32).asSInt.resize(64).asUInt
}

case class InsnBuffer(c: InsnBufferConfig) extends Component {
  val mem = Mem(Bits(64 bits), scala.math.pow(2, c.addrWidth).toInt)

  val io = new Bundle {
    val refill = slave Flow (InsnBufferRefillReq(c))
    val readReq = slave Stream (InsnBufferReadReq(c))
    val readRsp = master Stream (InsnBufferReadRsp(c))
  }

  // `io.readReq` is special in that it can have its payload mutated when valid && !ready.
  // Special care needs to be taken here - prevent the forks from going out of sync.
  val (readReqToBeStaged, readReqNow) = StreamFork2(io.readReq)
  val readReqStaged = readReqToBeStaged.stage().stage()
  val readData =
    mem
      .streamReadSync(readReqNow.translateWith(readReqNow.payload.addr))
      .stage()

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
