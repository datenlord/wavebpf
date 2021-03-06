package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

case class Controller(
    insnBufferConfig: InsnBufferConfig,
    regfetchConfig: RegfetchConfig,
    context: PeContextData
) extends Component {
  val io = new Bundle {
    val mmio = slave(AxiLite4(MMIOBusConfigV2()))
    val refill = master Flow (InsnBufferRefillReq(insnBufferConfig))
    val pcUpdater = master Flow (PcUpdateReq())
    val excReport = in(new CpuException())
    val excAck = out(Bool())
    val commitFire = in(Bool())
    val rfReplicaReadReq = master Stream (UInt(4 bits))
    val rfReplicaReadRsp = slave Stream (Bits(64 bits))
    val rfWriteOverride =
      master Flow (RegContext(regfetchConfig, Bits(64 bits)))
  }

  val perfCounter_cycles = Reg(UInt(64 bits)) init (0)
  val perfCounter_commits = Reg(UInt(64 bits)) init (0)

  perfCounter_cycles := perfCounter_cycles + 1
  when(io.commitFire) {
    perfCounter_commits := perfCounter_commits + 1
  }

  val excAckReg = Reg(Bool()) init (False)
  io.excAck := excAckReg

  val refillCounter = Reg(UInt(insnBufferConfig.addrWidth bits)) init (0)
  val buffer = Reg(Bits(32 bits))
  io.pcUpdater.payload.assignDontCare()
  io.pcUpdater.valid := False

  io.refill.setIdle()

  val mmio = WbpfUtil.axilite4Pipe(io.mmio)
  mmio.ar.setBlocked()
  mmio.aw.setBlocked()
  mmio.w.setBlocked()
  mmio.r.setIdle()
  mmio.b.setIdle()

  val awSnapshot = Reg(AxiLite4Ax(MMIOBusConfigV2()))
  val writeAddr = awSnapshot.addr(11 downto 2)

  def flushPc(pc: UInt, reason: SpinalEnumCraft[PcFlushReasonCode.type]): Unit = {
    io.pcUpdater.valid := True
    io.pcUpdater.payload.pc := pc
    io.pcUpdater.payload.flush := True
    io.pcUpdater.payload.flushReason := reason
    io.pcUpdater.payload.branchSourceValid := False
    io.pcUpdater.payload.branchSource.assignDontCare()
  }

  io.rfWriteOverride.setIdle()

  val writeFsm = new StateMachine {
    val waitForAw: State = new State with EntryPoint {
      whenIsActive {
        when(mmio.aw.valid) {
          mmio.aw.ready := True
          awSnapshot := mmio.aw.payload
          goto(waitForW)
        }
      }
    }
    val waitForW: State = new State {
      whenIsActive {
        when(mmio.w.valid) {
          mmio.w.ready := True
          switch(writeAddr) {
            is(0x00) {
              val value = mmio.w.payload
                .data(insnBufferConfig.addrWidth - 1 downto 0)
                .asUInt
              refillCounter := value
            }
            is(0x01) {
              flushPc(U(0, 29 bits), PcFlushReasonCode.STOP)
            }
            is(0x02) {
              buffer := mmio.w.payload.data
            }
            is(0x04) {
              val data = mmio.w.payload.data ## buffer
              io.refill.valid := True
              io.refill.payload.addr := refillCounter
              io.refill.payload.insn := data
              refillCounter := refillCounter + 1
            }
            is(0x06) {
              flushPc((mmio.w.payload.data >> 3).asUInt, PcFlushReasonCode.EXTERNAL)
            }
            is(0x08) {
              // Exception ACK
              when(io.excReport.valid) {
                excAckReg := io.excReport.generation
              }
            }
            is(0x10) {
              perfCounter_cycles := 0
              perfCounter_commits := 0
            }
            is(M"00001----1") {
              // Mapped registers
              mmio.r.valid := False
              io.rfWriteOverride.valid := True
              io.rfWriteOverride.payload.index := writeAddr(4 downto 1)
              io.rfWriteOverride.payload.data := mmio.w.payload.data ## buffer
            }
          }
          goto(sendWriteRsp)
        }
      }
    }
    val sendWriteRsp: State = new State {
      whenIsActive {
        mmio.b.valid := True
        mmio.b.payload.resp := 0 // OKAY
        when(mmio.b.ready) {
          goto(waitForAw)
        }
      }
    }
  }

  val arSnapshot = Reg(AxiLite4Ax(MMIOBusConfigV2()))
  val readAddr = arSnapshot.addr(11 downto 2)

  io.rfReplicaReadReq.setIdle()
  io.rfReplicaReadRsp.ready := mmio.r.ready

  val readFsm = new StateMachine {
    val mappedRegReadWait = Reg(Bool()) init (False)

    val waitForAr: State = new State with EntryPoint {
      whenIsActive {
        when(mmio.ar.valid) {
          mmio.ar.ready := True
          arSnapshot := mmio.ar.payload
          goto(sendReadRsp)
        }
      }
    }
    val sendReadRsp: State = new State {
      whenIsActive {
        mmio.r.valid := True
        mmio.r.payload.resp := 0 // OKAY

        switch(readAddr) {
          is(0x00) {
            mmio.r.payload.data := refillCounter.asBits.resized
          }
          is(0x03) {
            // Insn buffer size
            mmio.r.payload.data := BigInt("1") << insnBufferConfig.addrWidth
          }
          is(0x06) {
            val pc = io.excReport.pc.asBits << 3
            val data = pc(31 downto 0)
            mmio.r.payload.data := io.excReport.valid.mux(
              (False, U(0, 32 bits).asBits),
              (True, data)
            )
          }
          is(0x08) {
            val hasIntr =
              io.excReport.valid && (excAckReg =/= io.excReport.generation)
            mmio.r.payload.data := hasIntr.asBits ## io.excReport.valid.mux(
              (False, U(0, 31 bits).asBits),
              (True, io.excReport.code.asBits.resize(31 bits))
            )
          }
          is(0x0a) {
            val data = io.excReport.data(31 downto 0)
            mmio.r.payload.data := io.excReport.valid.mux(
              (False, U(0, 32 bits).asBits),
              (True, data.asBits)
            )
          }
          is(0x0b) {
            val data = io.excReport.data(63 downto 32)
            mmio.r.payload.data := io.excReport.valid.mux(
              (False, U(0, 32 bits).asBits),
              (True, data.asBits)
            )
          }
          is(0x0c) {
            val data =
              U(context.numPe, 16 bits) ## U(context.coreIndex, 16 bits)
            mmio.r.payload.data := data.asBits
          }
          is(0x0d) {
            mmio.r.payload.data := WbpfUtil.hwRevision
          }
          is(0x10) {
            mmio.r.payload.data := perfCounter_cycles(31 downto 0).asBits
          }
          is(0x11) {
            mmio.r.payload.data := perfCounter_cycles(63 downto 32).asBits
          }
          is(0x12) {
            mmio.r.payload.data := perfCounter_commits(31 downto 0).asBits
          }
          is(0x13) {
            mmio.r.payload.data := perfCounter_commits(63 downto 32).asBits
          }
          is(M"00001-----") {
            // Mapped registers
            mmio.r.valid := False
            when(mappedRegReadWait) {
              when(io.rfReplicaReadRsp.valid) {
                mmio.r.valid := True
                when(readAddr(0)) {
                  // upper half
                  mmio.r.payload.data := io.rfReplicaReadRsp.payload(
                    63 downto 32
                  )
                } otherwise {
                  mmio.r.payload.data := io.rfReplicaReadRsp.payload(
                    31 downto 0
                  )
                }
              }
            } otherwise {
              io.rfReplicaReadReq.valid := True
              io.rfReplicaReadReq.payload := readAddr(4 downto 1)
              when(io.rfReplicaReadReq.ready) {
                mappedRegReadWait := True
              }
            }
          }
          default {
            mmio.r.payload.data := 0
          }
        }
        when(mmio.r.fire) {
          mappedRegReadWait := False
          goto(waitForAr)
        }
      }
    }
  }
}

object MMIOBusConfigV2 {
  def apply() = AxiLite4Config(
    addressWidth = 32,
    dataWidth = 32
  )
}
