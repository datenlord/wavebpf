package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._

case class Controller(
    insnBufferConfig: InsnBufferConfig
) extends Component {
  val io = new Bundle {
    val mmio = slave(Axi4(MMIOBusConfigV2()))
    val refill = master Flow (InsnBufferRefillReq(insnBufferConfig))
    val pcUpdater = master Flow (PcUpdateReq())
  }

  val refillCounter = Reg(UInt(insnBufferConfig.addrWidth bits)) init (0)
  val refillBuffer = Reg(Bits(32 bits))
  io.pcUpdater.payload.assignDontCare()
  io.pcUpdater.valid := False

  io.refill.setIdle()
  io.mmio.setBlocked()

  val awSnapshot = Reg(Axi4Aw(MMIOBusConfigV2()))
  val writeAddr = awSnapshot.addr(7 downto 3)

  val writeFsm = new StateMachine {
    val waitForAw: State = new State with EntryPoint {
      whenIsActive {
        when(io.mmio.aw.valid) {
          awSnapshot := io.mmio.aw.payload
          goto(waitForW)
        }
      }
      onExit {
        io.mmio.aw.ready := True
      }
    }
    val waitForW: State = new State {
      whenIsActive {
        when(io.mmio.w.valid) {
          io.mmio.w.ready := True
          when(io.mmio.w.last) {
            switch(writeAddr) {
              is(0x00) {
                val value = io.mmio.w.payload
                  .data(insnBufferConfig.addrWidth - 1 downto 0)
                  .asUInt
                refillCounter := value
              }
              is(0x01) {
                refillBuffer := io.mmio.w.payload.data
              }
              is(0x02) {
                val data = io.mmio.w.payload.data ## refillBuffer
                io.refill.valid := True
                io.refill.payload.addr := refillCounter
                io.refill.payload.insn := data
                refillCounter := refillCounter + 1
              }
              is(0x03) {
                io.pcUpdater.valid := True
                io.pcUpdater.payload.pc := io.mmio.w.payload.data.asUInt.resized
                io.pcUpdater.payload.flush := True
                io.pcUpdater.payload.flushReason := PcFlushReasonCode.EXTERNAL
                report(Seq("Update PC: ", io.mmio.w.payload.data.asUInt))
              }
            }
            goto(sendWriteRsp)
          }
        }
      }
    }
    val sendWriteRsp: State = new State {
      whenIsActive {
        io.mmio.b.valid := True
        io.mmio.b.payload.id := awSnapshot.id
        when(io.mmio.b.ready) {
          goto(waitForAw)
        }
      }
    }
  }
}

object MMIOBusConfigV2 {
  def apply() = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 4
  )
}
