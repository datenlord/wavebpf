package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._

object MemoryAccessWidth extends SpinalEnum(binarySequential) {
  val W1, W2, W4, W8 = newElement()

  def alignedMask(self: SpinalEnumCraft[this.type]): Bits = {
    val x = Bits(8 bits)
    switch(self) {
      is(W1) {
        x := 0x1
      }
      is(W2) {
        x := 0x3
      }
      is(W4) {
        x := 0xf
      }
      is(W8) {
        x := 0xff
      }
    }
    x
  }
}

case class ExecConfig(
    insnFetch: InsnBufferConfig,
    regFetch: RegfetchConfig,
    splitAluMem: Boolean,
    reportCommit: Boolean,
    bypassMemOutput: Boolean,
    context: PeContextData,
    useBtbForConditionalBranches: Boolean
)

case class AluStageInsnContext(
    c: ExecConfig
) extends Bundle {
  val regFetch = RegGroupContext(c.regFetch, Bits(64 bits))
  val insnFetch = InsnBufferReadRsp(c.insnFetch)
  val regWritebackValid = Bool()
  val regWriteback = RegContext(c.regFetch, Bits(64 bits))
  val exc = new CpuExceptionSkeleton()
  val memory = new MemoryAccessReq(c)
  val br = BranchReq()
}

case class MemoryAccessReq(
    c: ExecConfig
) extends Bundle {
  val valid = Bool()
  val store = Bool()
  val addr = UInt(32 bits)
  val rd = UInt(log2Up(c.regFetch.numRegs) bits)
  val width = MemoryAccessWidth()
}

case class MemoryStageInsnContext(
    c: ExecConfig
) extends Bundle {
  val regFetch = RegGroupContext(c.regFetch, Bits(64 bits))
  val insnFetch = InsnBufferReadRsp(c.insnFetch)
  val regWritebackValid = Bool()
  val regWriteback = RegContext(c.regFetch, Bits(64 bits))
  val br = BranchReq()
}

case class BranchReq() extends Bundle {
  val valid = Bool()
  val isConditional = Bool()
  val addr = UInt(29 bits)
}

case class Exec(c: ExecConfig) extends Component {
  val io = new Bundle {
    val regFetch = slave Stream (RegGroupContext(c.regFetch, Bits(64 bits)))
    val regWriteback = master Flow (RegContext(c.regFetch, Bits(64 bits)))
    val dataMem = master(DataMemV2Port())
    val insnFetch = slave Stream (InsnBufferReadRsp(c.insnFetch))
    val excOutput = out(new CpuException())
    val branchPcUpdater = master Flow (PcUpdateReq())
    val excAck = in(Bool())
    val commitFire = out(Bool())
  }

  val rs1Bypass =
    new BypassNetwork(UInt(log2Up(c.regFetch.numRegs) bits), Bits(64 bits))
  val rs2Bypass =
    new BypassNetwork(UInt(log2Up(c.regFetch.numRegs) bits), Bits(64 bits))
  val rs1MemStallBypass =
    new BypassNetwork(UInt(log2Up(c.regFetch.numRegs) bits), Bool)
  val rs2MemStallBypass =
    new BypassNetwork(UInt(log2Up(c.regFetch.numRegs) bits), Bool)

  rs1Bypass.srcKey := io.regFetch.rs1.index
  rs1Bypass.srcValue := io.regFetch.rs1.data
  rs2Bypass.srcKey := io.regFetch.rs2.index
  rs2Bypass.srcValue := io.regFetch.rs2.data
  rs1MemStallBypass.srcKey := io.regFetch.rs1.index
  rs1MemStallBypass.srcValue := False
  rs2MemStallBypass.srcKey := io.regFetch.rs2.index
  rs2MemStallBypass.srcValue := False

  val rfOverride = RegGroupContext(c.regFetch, Bits(64 bits))
  rfOverride.rs1.index := io.regFetch.rs1.index
  rfOverride.rs1.data := rs1Bypass.bypassed
  rfOverride.rs2.index := io.regFetch.rs2.index
  rfOverride.rs2.data := rs2Bypass.bypassed

  val aluStage = new ExecAluStage(c)
  aluStage.io.bypassEmpty := rs1Bypass.empty && rs2Bypass.empty
  io.regFetch
    .translateWith(rfOverride)
    .continueWhen(
      !rs1MemStallBypass.bypassed && !rs2MemStallBypass.bypassed
    ) >> aluStage.io.regFetch
  io.insnFetch >> aluStage.io.insnFetch
  val memStage =
    new ExecMemoryStage(
      c,
      Seq(rs1Bypass, rs2Bypass),
      Seq(rs1MemStallBypass, rs2MemStallBypass)
    )
  memStage.io.excAck := io.excAck

  val memOutputFlow = memStage.io.output.toFlow

  memStage.io.aluStage << aluStage.io.output
  memStage.io.dataMem.request >> io.dataMem.request
  memStage.io.dataMem.response << io.dataMem.response
  memOutputFlow
    .throwWhen(!memStage.io.output.payload.regWritebackValid)
    .translateWith(memStage.io.output.payload.regWriteback) >> io.regWriteback
  io.excOutput := memStage.io.excOutput

  val pcUpdateReq = PcUpdateReq()
  pcUpdateReq.pc := memStage.io.output.payload.br.addr
  pcUpdateReq.flush := True
  pcUpdateReq.flushReason := PcFlushReasonCode.BRANCH_RESOLVE
  pcUpdateReq.branchSourceValid := Bool(c.useBtbForConditionalBranches) || !memStage.io.output.payload.br.isConditional
  pcUpdateReq.branchSource := memStage.io.output.payload.insnFetch.addr.resized
  memOutputFlow
    .throwWhen(!memStage.io.output.payload.br.valid)
    .translateWith(pcUpdateReq) >> io.branchPcUpdater

  io.commitFire := memOutputFlow.fire

  if (c.reportCommit) {
    when(memOutputFlow.fire) {
      report(
        Seq(
          "COMMIT",
          " core=" + c.context.coreIndex,
          " pc=",
          memStage.io.output.payload.insnFetch.addr,
          " regwb=",
          memStage.io.output.payload.regWritebackValid,
          " regwb.index=",
          memStage.io.output.payload.regWriteback.index,
          " regwb.data=",
          memStage.io.output.payload.regWriteback.data
        )
      )
    }
  }
}

case class ExecMemoryStage(
    c: ExecConfig,
    bypass: Seq[BypassNetwork[UInt, Bits]],
    stallBypass: Seq[BypassNetwork[UInt, Bool]]
) extends Area {
  private def provideBypassResource(
      stage: Flow[AluStageInsnContext],
      subprio: Int
  ) {
    for (net <- bypass) {
      net.provide(
        2,
        subprio,
        stage.valid && stage.regWritebackValid && stage.regWriteback.index === net.srcKey,
        stage.regWriteback.data
      )
    }
    for (net <- stallBypass) {
      net.provide(
        2,
        subprio,
        stage.valid && stage.memory.rd === net.srcKey && stage.memory.valid && !stage.memory.store,
        True
      )
    }
  }
  val io = new Bundle {
    val aluStage = Stream(AluStageInsnContext(c))
    val dataMem = DataMemV2Port()
    val output = Stream(MemoryStageInsnContext(c))
    val excOutput = new CpuException()
    val excAck = Bool()
  }

  val outData = MemoryStageInsnContext(c)
  val memRead = Bits(64 bits)

  val aluOutput = Stream(AluStageInsnContext(c))
  if (c.splitAluMem) {
    val half = io.aluStage.stage()
    provideBypassResource(half.asFlow, 0)
    aluOutput << half.s2mPipe()
    provideBypassResource(aluOutput.asFlow, 1)
  } else {
    aluOutput << io.aluStage
  }

  val excRegInit = new CpuException()
  excRegInit.valid := True
  excRegInit.code := CpuExceptionCode.NOT_INIT
  excRegInit.data := 0
  excRegInit.pc := 0
  excRegInit.generation := False

  val nextExc = new CpuException()
  val excReg =
    RegNextWhen(next = nextExc, cond = aluOutput.fire, init = excRegInit)
  io.excOutput := excReg

  val wasBranch = Bool(false)

  when(
    aluOutput.valid && (
      (!excReg.valid && aluOutput.payload.exc.valid) || (excReg.valid && (
        aluOutput.payload.insnFetch.ctx.flush &&
          ControlFlowPriority.forFlushReason(
            aluOutput.payload.insnFetch.ctx.flushReason
          ) <=
          ControlFlowPriority.forException(excReg.code)
      ))
    )
  ) {
    nextExc.valid := aluOutput.payload.exc.valid
    nextExc.code := aluOutput.payload.exc.code
    nextExc.data := aluOutput.payload.exc.data
    nextExc.pc := aluOutput.payload.insnFetch.addr.resized
    nextExc.generation := !io.excAck
  } elsewhen (aluOutput.valid && !excReg.valid && aluOutput.payload.br.valid) {
    nextExc.valid := True
    nextExc.code := CpuExceptionCode.PENDING_BRANCH
    nextExc.data := 0
    nextExc.pc := aluOutput.payload.insnFetch.addr.resized
    nextExc.generation := !io.excAck // will be overrided in ProcessingElement
    wasBranch.set()
  } otherwise {
    nextExc := excReg
  }

  var maskedAluOutput = aluOutput.throwWhen(nextExc.valid && !wasBranch)

  val (maskedAluOutputToMem, maskedAluOutputToStage) = StreamFork2(
    maskedAluOutput
  )

  val storeData = Mux[Bits](
    sel = maskedAluOutput.insnFetch.insn(0),
    whenTrue = maskedAluOutput.regFetch.rs2.data,
    whenFalse = maskedAluOutput.insnFetch.imm.asBits
  )

  val memReq = DataMemRequest()
  memReq.addr := maskedAluOutput.memory.addr
  memReq.write := maskedAluOutput.memory.store
  memReq.ctx.assignDontCare()
  memReq.data := storeData
  memReq.width := maskedAluOutput.memory.width
  memReq.precomputedStrbValid := False
  memReq.precomputedStrb.assignDontCare()
  maskedAluOutputToMem
    .throwWhen(!maskedAluOutputToMem.memory.valid)
    .translateWith(memReq) >> io.dataMem.request

  val maskedAluOutputStaged = maskedAluOutputToStage.stage()

  val writebackOverride = RegContext(c.regFetch, Bits(64 bits))
  writebackOverride.index := maskedAluOutputStaged.memory.rd
  writebackOverride.data := io.dataMem.response.payload.data

  outData.insnFetch := maskedAluOutputStaged.insnFetch
  outData.regFetch := maskedAluOutputStaged.regFetch

  val shouldWriteback =
    maskedAluOutputStaged.memory.valid && !maskedAluOutputStaged.memory.store
  outData.regWritebackValid := shouldWriteback
    .mux(
      (False, maskedAluOutputStaged.regWritebackValid),
      (True, True)
    )
  outData.regWriteback := shouldWriteback
    .mux(
      (False, maskedAluOutputStaged.regWriteback),
      (True, writebackOverride)
    )
  outData.br := maskedAluOutputStaged.br

  val dmRspAlwaysValid = Stream(DataMemResponse())
  dmRspAlwaysValid.valid := True
  dmRspAlwaysValid.payload.assignDontCare()

  // Do not wait for memory response if we did not issue a request
  val dmRspMux = StreamMux(
    maskedAluOutputStaged.memory.valid
      .mux((False, U(0, 1 bits)), (True, U(1, 1 bits))),
    Vec(dmRspAlwaysValid, io.dataMem.response)
  )

  val output = StreamJoin(maskedAluOutputStaged, dmRspMux)
    .translateWith(outData)

  io.output << output

  val bypassCtx = AluStageInsnContext(c)
  bypassCtx := maskedAluOutputStaged

  if (c.bypassMemOutput) {
    when(output.valid) {
      bypassCtx.regWritebackValid := output.regWritebackValid
      bypassCtx.regWriteback := output.payload.regWriteback
      bypassCtx.memory.valid := False
    }
  }

  provideBypassResource(
    maskedAluOutputStaged.asFlow.translateWith(bypassCtx),
    2
  )
}

case class ExecAluStage(c: ExecConfig) extends Component {
  val io = new Bundle {
    val regFetch = slave Stream (RegGroupContext(c.regFetch, Bits(64 bits)))
    val insnFetch = slave Stream (InsnBufferReadRsp(c.insnFetch))
    val output = master Stream (AluStageInsnContext(c))
    val bypassEmpty = in Bool ()
  }

  val opcode = io.insnFetch.payload.insn(7 downto 0)
  val rdIndex = io.insnFetch.payload.insn(11 downto 8).asUInt
  val rs2Index = io.regFetch.payload.rs2.index
  val imm = io.insnFetch.payload.imm
  val rs1 = io.regFetch.payload.rs1.data.asUInt
  val rs2 = io.regFetch.payload.rs2.data.asUInt
  val offset32 =
    io.insnFetch.payload.insn(31 downto 16).asSInt.resize(32).asUInt

  val operand2IsReg = opcode(3)
  val operand2 = Mux(sel = operand2IsReg, whenTrue = rs2, whenFalse = imm)

  val exc = new CpuExceptionSkeleton()
  exc.code.assignDontCare()
  exc.data.assignDontCare()
  exc.valid := False

  val memory = new MemoryAccessReq(c)
  memory.assignDontCare()
  memory.valid := False
  memory.rd := rdIndex

  val regWritebackData = RegContext(c.regFetch, Bits(64 bits))
  regWritebackData.assignDontCare()
  regWritebackData.index := 0
  val regWritebackValid = Bool()
  regWritebackValid := False

  val isStore = opcode(1)
  memory.store := isStore
  memory.addr := isStore.mux(
    (False, rs2.resize(32 bits)),
    (True, rs1.resize(32 bits))
  ) + offset32

  val br = BranchReq()
  br.valid := False
  br.isConditional := True
  br.addr := io.insnFetch.payload.addr.resize(29 bits) + 1 + offset32.resize(
    29 bits
  )

  val condEq = rs1 === operand2
  val condNe = rs1 =/= operand2
  val condLt = rs1 < operand2
  val condLe = condLt || condEq
  val condGt = !condLe
  val condGe = !condLt
  val condSlt = rs1.asSInt < operand2.asSInt
  val condSle = condSlt || condEq
  val condSgt = !condSle
  val condSge = !condSlt
  val isAlu = Bool()
  isAlu := False

  val likeAlu32 = !opcode(0) & !opcode(1)

  case class LddwHigherHalfState() extends Bundle {
    val valid = Bool()
    val regindex = UInt(log2Up(c.regFetch.numRegs) bits)
    val lower = Bits(32 bits)
  }
  val lddwHigherHalfInit = LddwHigherHalfState()
  lddwHigherHalfInit.assignDontCare()
  lddwHigherHalfInit.valid := False

  val lddwHigherHalfNext = LddwHigherHalfState()
  lddwHigherHalfNext.assignDontCare()
  lddwHigherHalfNext.valid := False

  val lddwHigherHalfReg =
    RegNextWhen(
      next = lddwHigherHalfNext,
      cond = io.output.fire,
      init = lddwHigherHalfInit
    )

  val lddwHigherHalf = LddwHigherHalfState()
  lddwHigherHalf.valid := lddwHigherHalfReg.valid & !io.insnFetch.payload.ctx.flush
  lddwHigherHalf.assignUnassignedByName(lddwHigherHalfReg)

  def reportBadInsn() {
    exc.valid := True
    exc.code := CpuExceptionCode.BAD_INSTRUCTION
    exc.data := io.insnFetch.payload.insn.asUInt
  }

  switch(opcode) {
    is(M"0000-111", M"0000-100") { // 0x07/0x0f, dst += imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 + operand2).asBits
    }
    is(M"0001-111", M"0001-100") { // 0x17/0x1f, dst -= imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 - operand2).asBits
    }
    is(M"0010-111", M"0010-100") { // 0x27/0x2f, dst *= imm
      /*
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 * operand2)(63 downto 0).asBits
       */
      // multiplication not implemented
      reportBadInsn()
    }
    is(M"0011-111", M"0011-100") { // 0x37/0x3f, dst /= imm
      // division not implemented
      reportBadInsn()
    }
    is(M"0100-111", M"0100-100") { // 0x47/0x4f, dst |= imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 | operand2).asBits
    }
    is(M"0101-111", M"0101-100") { // 0x57/0x5f, dst &= imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 & operand2).asBits
    }
    is(M"0110-111", M"0110-100") { // 0x67/0x6f, dst <<= imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 << operand2(5 downto 0))(63 downto 0).asBits
    }
    is(M"0111-111", M"0111-100") { // 0x77/0x7f, dst >>= imm (logical)
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 >> operand2(5 downto 0))(63 downto 0).asBits
    }
    is(M"10000111", M"10000100") { // 0x87, dst = -dst
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (-rs1.asSInt).asBits
    }
    is(M"1001-111", M"1001-100") { // 0x97/0x9f, dst %= imm
      // division not implemented
      reportBadInsn()
    }
    is(M"1010-111", M"1010-100") { // 0xa7/0xaf, dst ^= imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1 ^ operand2).asBits
    }
    is(M"1011-111", M"1011-100") { // 0xb7/0xbf, dst = imm
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := operand2.asBits
    }
    is(M"1100-111", M"1100-100") { // 0xc7/0xcf, dst >>= imm (arithmetic)
      isAlu := True
      regWritebackValid := True
      regWritebackData.index := rdIndex
      regWritebackData.data := (rs1.asSInt >> operand2(5 downto 0))(
        63 downto 0
      ).asBits
    }
    is(0x18) { // dst = imm
      lddwHigherHalfNext.valid := True
      lddwHigherHalfNext.regindex := rdIndex
      lddwHigherHalfNext.lower := imm(31 downto 0).asBits
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

    // Branch ops
    is(0x05) {
      // PC += off
      br.valid := True
      br.isConditional := False

      // SP adjustment
      regWritebackValid := True
      regWritebackData.index := 10
      regWritebackData.data := (rs1 + imm).asBits
    }
    is(0x15, 0x1d) {
      // jeq
      br.valid := condEq
    }
    is(0x25, 0x2d) {
      // jgt
      br.valid := condGt
    }
    is(0x35, 0x3d) {
      // jge
      br.valid := condGe
    }
    is(0xa5, 0xad) {
      // jlt
      br.valid := condLt
    }
    is(0xb5, 0xbd) {
      // jle
      br.valid := condLe
    }
    is(0x55, 0x5d) {
      // jne
      br.valid := condNe
    }
    is(0x65, 0x6d) {
      // jsgt
      br.valid := condSgt
    }
    is(0x75, 0x7d) {
      // jsge
      br.valid := condSge
    }
    is(0xc5, 0xcd) {
      // jslt
      br.valid := condSlt
    }
    is(0xd5, 0xdd) {
      // jsle
      br.valid := condSle
    }
    is(0x95) {
      // exit
      exc.valid := True
      exc.code := CpuExceptionCode.EXIT
      exc.data := rs1
    }
    is(0x85) {
      // call
      switch(imm(7 downto 0)) {
        is(0x01) {
          // get core index
          regWritebackValid := True
          regWritebackData.index := 0
          regWritebackData.data := U(0, 32 bits).asBits ## U(
            c.context.numPe,
            16 bits
          ).asBits ## U(
            c.context.coreIndex,
            16 bits
          ).asBits
        }
        default {
          // exception
          exc.valid := True
          exc.code := CpuExceptionCode.CALL
          exc.data := imm
        }
      }
    }
    is(0x00) {
      // lddw higher half
      when(lddwHigherHalf.valid) {
        regWritebackValid := True
        regWritebackData.index := lddwHigherHalf.regindex
        regWritebackData.data := imm(31 downto 0) ## lddwHigherHalf.lower
      } otherwise {
        reportBadInsn()
      }
    }
    default {
      reportBadInsn()
    }
  }

  val regWritebackOverride = RegContext(c.regFetch, Bits(64 bits))
  regWritebackOverride.index := regWritebackData.index
  regWritebackOverride.data := Mux(
    sel = isAlu & likeAlu32,
    whenTrue = regWritebackData.data(31 downto 0).resize(64 bits),
    whenFalse = regWritebackData.data
  )

  // Do not issue a branch if our prediction is correct
  val brOverride = BranchReq()
  val sequentialNextPc = io.insnFetch.payload.addr + 1
  when(
    br.valid &&
      io.insnFetch.payload.ctx.prediction.valid &&
      io.insnFetch.payload.ctx.prediction.predictedTarget === br.addr.resized
  ) {
    /*report(
      Seq(
        "Suppressing branch request due to prediction hit - ",
        io.insnFetch.payload.addr,
        " -> ",
        br.addr
      )
    )*/
    brOverride.valid := False
    brOverride.addr.assignDontCare()
    brOverride.isConditional.assignDontCare()
  } elsewhen (
    !br.valid &&
      io.insnFetch.payload.ctx.prediction.valid &&
      io.insnFetch.payload.ctx.prediction.predictedTarget =/= sequentialNextPc
  ) {
    /*report(
      Seq(
        "Fixing up false-positive branch - ",
        io.insnFetch.payload.addr,
        " -> ",
        io.insnFetch.payload.ctx.prediction.predictedTarget,
        ", should be ",
        sequentialNextPc
      )
    )*/
    brOverride.valid := True
    brOverride.addr := sequentialNextPc.resized
    brOverride.isConditional := False
  } otherwise {
    brOverride := br
  }

  val ctxOut = AluStageInsnContext(c)
  ctxOut.regFetch := io.regFetch
  ctxOut.insnFetch := io.insnFetch
  ctxOut.regWritebackValid := regWritebackValid
  ctxOut.regWriteback := regWritebackOverride
  ctxOut.exc := exc
  ctxOut.memory := memory
  ctxOut.br := brOverride

  when(
    io.insnFetch.payload.ctx.flush && io.insnFetch.payload.ctx.flushReason === PcFlushReasonCode.STOP
  ) {
    val stopExc = new CpuExceptionSkeleton()
    stopExc.valid := True
    stopExc.code := CpuExceptionCode.STOP
    stopExc.data := 0
    ctxOut.exc := stopExc
  }

  // An instruction that has the `flush` flag set can re-activate a pipeline blocked by an
  // exception. At this point there may still be some data in the bypassed pipeline registers,
  // but it is invalid to use it. Here, we wait for the pipeline registers to empty out.
  val outStream = StreamJoin
    .arg(io.regFetch, io.insnFetch)
    .continueWhen(!io.insnFetch.payload.ctx.flush || io.bypassEmpty)
    .translateWith(ctxOut)

  io.output << outStream

  /*when(!outStream.valid) {
    report(L"exec alu stall")
  }*/

  /*when(outStream.valid && memory.valid) {
    report(
      Seq(
        "Memory access at ",
        memory.addr,
        " with width ",
        memory.width,
        " and store ",
        memory.store
      )
    )
  }*/
}
