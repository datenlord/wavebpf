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
    val excReport = in(new CpuException())
  }

  val refillCounter = Reg(UInt(insnBufferConfig.addrWidth bits)) init (0)
  val refillBuffer = Reg(Bits(32 bits))
  io.pcUpdater.payload.assignDontCare()
  io.pcUpdater.valid := False

  io.refill.setIdle()

  val mmio = Axi4(MMIOBusConfigV2())
  mmio.ar << io.mmio.ar.s2mPipe()
  mmio.aw << io.mmio.aw.s2mPipe()
  mmio.w << io.mmio.w.s2mPipe()
  mmio.r >> io.mmio.r
  mmio.b >> io.mmio.b
  mmio.setBlocked()

  val awSnapshot = Reg(Axi4Aw(MMIOBusConfigV2()))
  val writeAddr = awSnapshot.addr(11 downto 3)

  mmio.b.payload.id := awSnapshot.id

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
          when(mmio.w.last) {
            switch(writeAddr) {
              is(0x00) {
                val value = mmio.w.payload
                  .data(insnBufferConfig.addrWidth - 1 downto 0)
                  .asUInt
                refillCounter := value
              }
              is(0x01) {
                refillBuffer := mmio.w.payload.data
              }
              is(0x02) {
                val data = mmio.w.payload.data ## refillBuffer
                io.refill.valid := True
                io.refill.payload.addr := refillCounter
                io.refill.payload.insn := data
                refillCounter := refillCounter + 1
              }
              is(0x03) {
                io.pcUpdater.valid := True
                io.pcUpdater.payload.pc := mmio.w.payload.data.asUInt.resized
                io.pcUpdater.payload.flush := True
                io.pcUpdater.payload.flushReason := PcFlushReasonCode.EXTERNAL
                report(Seq("Update PC: ", mmio.w.payload.data.asUInt))
              }
            }
            goto(sendWriteRsp)
          }
        }
      }
    }
    val sendWriteRsp: State = new State {
      whenIsActive {
        mmio.b.valid := True
        when(mmio.b.ready) {
          goto(waitForAw)
        }
      }
    }
  }

  val arSnapshot = Reg(Axi4Ar(MMIOBusConfigV2()))
  val readAddr = arSnapshot.addr(11 downto 3)
  val readHigherHalf = arSnapshot.addr(2)

  val readFsm = new StateMachine {
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
        mmio.r.payload.id := arSnapshot.id
        mmio.r.payload.last := True

        switch(readAddr) {
          is(0x00) {
            mmio.r.payload.data := refillCounter.asBits.resized
          }
          is(0x03) {
            val pc = io.excReport.pc.asBits << 3
            val data = readHigherHalf.mux(
              (False, pc(31 downto 0)),
              (True, pc(63 downto 32))
            )
            mmio.r.payload.data := io.excReport.valid.mux(
              (False, U(0, 32 bits).asBits),
              (True, data)
            )
          }
          is(0x04) {
            mmio.r.payload.data := io.excReport.valid.mux(
              (False, U(0, 32 bits).asBits),
              (True, io.excReport.code.asBits.resize(32 bits))
            )
          }
          is(0x05) {
            val data = readHigherHalf.mux(
              (False, io.excReport.data(31 downto 0)),
              (True, io.excReport.data(63 downto 32))
            )
            mmio.r.payload.data := io.excReport.valid.mux(
              (False, U(0, 32 bits).asBits),
              (True, data.asBits)
            )
          }
          default {
            mmio.r.payload.data := 0
          }
        }
        when(mmio.r.ready) {
          goto(waitForAr)
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
