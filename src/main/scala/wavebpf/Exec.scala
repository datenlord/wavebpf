package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._

object MemoryAccessWidth extends SpinalEnum(binarySequential) {
  val W1, W2, W4, W8 = newElement()
}

case class ExecConfig(
    insnFetch: InsnBufferConfig,
    regFetch: RegfetchConfig,
    splitAluMem: Boolean
)

case class AluStageInsnContext(
    c: ExecConfig
) extends Bundle {
  val regFetch = RegGroupContext(c.regFetch, Bits(64 bits))
  val insnFetch = InsnBufferReadRsp(c.insnFetch)
  val regWriteback = RegContext(c.regFetch, Bits(64 bits))
  val exc = CpuException()
  val memory = new MemoryAccessReq()
}

case class MemoryAccessReq() extends Bundle {
  val valid = Bool()
  val store = Bool()
  val addr = UInt(64 bits)
  val width = MemoryAccessWidth()
}

case class MemoryStageInsnContext(
    c: ExecConfig
) extends Bundle {
  val regFetch = RegGroupContext(c.regFetch, Bits(64 bits))
  val insnFetch = InsnBufferReadRsp(c.insnFetch)
  val regWriteback = RegContext(c.regFetch, Bits(64 bits))
  val exc = CpuException()
}

case class Exec(c: ExecConfig) extends Component {
  val io = new Bundle {
    val regFetch = slave Stream (RegGroupContext(c.regFetch, Bits(64 bits)))
    val regWriteback = master Flow (RegContext(c.regFetch, Bits(64 bits)))
    val dataMem = master(Wishbone(DataMemWishboneConfig()))
    val insnFetch = slave Stream (InsnBufferReadRsp(c.insnFetch))
  }

  val aluStage = new ExecAluStage(c)
  io.regFetch >> aluStage.io.regFetch
  io.insnFetch >> aluStage.io.insnFetch
  val memStage = new ExecMemoryStage(c)
  memStage.io.aluStage << aluStage.io.output
  memStage.io.dataMem >> io.dataMem
  memStage.io.output
    .translateWith(memStage.io.output.payload.regWriteback)
    .toFlow >> io.regWriteback
}

case class ExecMemoryStage(c: ExecConfig) extends Component {
  val io = new Bundle {
    val aluStage = slave Stream (AluStageInsnContext(c))
    val dataMem = master(Wishbone(DataMemWishboneConfig()))
    val output = master Stream (MemoryStageInsnContext(c))
  }

  val out = MemoryStageInsnContext(c)
  val memRead = Bits(64 bits)
  val wasInException = Reg(Bool) init (True) // startup
  val inException = wasInException && !io.aluStage.payload.insnFetch.ctx.flush
  wasInException := inException || io.aluStage.payload.exc.valid

  val fifo = StreamFifoLowLatency(AluStageInsnContext(c), 1)

  fifo.io.push << io.aluStage.throwWhen(inException)
  fifo.io.pop.ready := io.output.ready && (!fifo.io.pop.payload.memory.valid || io.dataMem.ACK)

  io.dataMem.CYC := fifo.io.pop.valid && fifo.io.pop.payload.memory.valid
  io.dataMem.STB := fifo.io.pop.valid && fifo.io.pop.payload.memory.valid
  io.dataMem.ADR := fifo.io.pop.payload.memory.addr
  io.dataMem.WE := fifo.io.pop.payload.memory.store
  io.dataMem.DAT_MOSI := fifo.io.pop.payload.regFetch.rs2.data // TODO: imm

  val writebackOverride = RegContext(c.regFetch, Bits(64 bits))
  writebackOverride.index := fifo.io.pop.payload.regWriteback.index
  writebackOverride.data := io.dataMem.DAT_MISO

  out.insnFetch := fifo.io.pop.payload.insnFetch
  out.regFetch := fifo.io.pop.payload.regFetch
  out.regWriteback := (fifo.io.pop.payload.memory.valid && !fifo.io.pop.payload.memory.store)
    .mux(
      (False, fifo.io.pop.payload.regWriteback),
      (True, writebackOverride)
    )
  out.exc := fifo.io.pop.payload.exc
  io.output.valid := fifo.io.pop.valid && fifo.io.pop.ready
  io.output.payload := out
}

case class ExecAluStage(c: ExecConfig) extends Component {
  val io = new Bundle {
    val regFetch = slave Stream (RegGroupContext(c.regFetch, Bits(64 bits)))
    val insnFetch = slave Stream (InsnBufferReadRsp(c.insnFetch))
    val output = master Stream (AluStageInsnContext(c))
  }

  val opcode = io.insnFetch.payload.insn(7 downto 0)
  val rdIndex = io.insnFetch.payload.insn(11 downto 8).asUInt
  val imm = io.insnFetch.payload.insn(63 downto 32).asSInt.resize(64).asUInt
  val rs1 = io.regFetch.payload.rs1.data.asUInt
  val rs2 = io.regFetch.payload.rs2.data.asUInt
  val offset = io.insnFetch.payload.insn(31 downto 16).asSInt.resize(64).asUInt

  val operand2 = UInt(64 bits)
  when(opcode(3)) {
    operand2 := rs2
  } otherwise {
    operand2 := imm
  }

  val exc = CpuException()
  exc.assignDontCare()
  exc.valid := False

  val memory = new MemoryAccessReq()
  memory.assignDontCare()
  memory.valid := False

  val regWritebackData = RegContext(c.regFetch, Bits(64 bits))
  regWritebackData.assignDontCare()
  regWritebackData.index := 0

  val isStore = opcode(1)
  memory.store := isStore
  memory.addr := isStore.mux(
    (False, rs2),
    (True, rs1)
  ) + offset

  switch(opcode) {
    is(M"0000-111") { // 0x07/0x0f, dst += imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 + operand2).asBits
    }
    is(M"0001-111") { // 0x17/0x1f, dst -= imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 - operand2).asBits
    }
    is(M"0010-111") { // 0x27/0x2f, dst *= imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 * operand2)(63 downto 0).asBits
    }
    is(M"0011-111") { // 0x37/0x3f, dst /= imm
      // division not implemented
      exc.valid := True
      exc.code := CpuExceptionCode.BAD_INSTRUCTION
      exc.data := 0
    }
    is(M"0100-111") { // 0x47/0x4f, dst |= imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 | operand2).asBits
    }
    is(M"0101-111") { // 0x57/0x5f, dst &= imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 & operand2).asBits
    }
    is(M"0110-111") { // 0x67/0x6f, dst <<= imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 << operand2(5 downto 0))(63 downto 0).asBits
    }
    is(M"0111-111") { // 0x77/0x7f, dst >>= imm (logical)
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 >> operand2(5 downto 0))(63 downto 0).asBits
    }
    is(M"10000111") { // 0x87, dst = -dst
      regWritebackData.index := rdIndex
      regWritebackData.data := (-rs1.asSInt).asBits
    }
    is(M"1001-111") { // 0x97/0x9f, dst %= imm
      // division not implemented
      exc.valid := True
      exc.code := CpuExceptionCode.BAD_INSTRUCTION
      exc.data := 0
    }
    is(M"1010-111") { // 0xa7/0xaf, dst ^= imm
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 ^ operand2).asBits
    }
    is(M"1011-111") { // 0xb7/0xbf, dst = imm
      regWritebackData.index := rdIndex
      regWritebackData.data := operand2.asBits
    }
    is(M"1100-111") { // 0xc7/0xcf, dst >>= imm (arithmetic)
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1.asSInt >> operand2(5 downto 0))(63 downto 0).asBits
    }
    is(0x18) { // dst = imm
      regWritebackData.index := rdIndex
      regWritebackData.data := imm.asBits
    }
    is(0x61, 0x62, 0x63) {
      memory.valid := True
      memory.width := MemoryAccessWidth.W4
    }
    is(0x69, 0x6a, 0x6b) {
      memory.valid := True
      memory.width := MemoryAccessWidth.W2
    }
    is(0x71, 0x72, 0x73) {
      memory.valid := True
      memory.width := MemoryAccessWidth.W1
    }
    is(0x79, 0x7a, 0x7b) {
      memory.valid := True
      memory.width := MemoryAccessWidth.W8
    }
    default {
      exc.valid := True
      exc.code := CpuExceptionCode.BAD_INSTRUCTION
      exc.data := 0
    }
  }

  val ctxOut = AluStageInsnContext(c)
  ctxOut.regFetch := io.regFetch
  ctxOut.insnFetch := io.insnFetch
  ctxOut.regWriteback := regWritebackData
  ctxOut.exc := exc
  ctxOut.memory := memory

  io.output << StreamJoin
    .arg(io.regFetch, io.insnFetch)
    .translateWith(ctxOut)
}
