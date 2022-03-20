package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._

case class Controller(
    insnBufferConfig: InsnBufferConfig
) extends Component {
  val io = new Bundle {
    val mmio = slave(Wishbone(MMIOBusConfig()))
    val refill = master Flow (InsnBufferRefillReq(insnBufferConfig))
    val pcUpdater = master Flow(PcUpdateReq())
  }

  val refillCounter = Reg(UInt(insnBufferConfig.addrWidth bits)) init (0)
  val refillBuffer = Reg(Bits(32 bits))
  io.pcUpdater.payload.assignDontCare()
  io.pcUpdater.valid := False

  io.refill.setIdle()
  io.mmio.clearAll()

  val addr = io.mmio.ADR(3 downto 0)
  when(io.mmio.STB) {
    io.mmio.ACK := True
    switch(addr) {
      is(0x00) {
        when(io.mmio.WE) {
          val value = io.mmio
            .DAT_MOSI(insnBufferConfig.addrWidth - 1 downto 0)
            .asUInt
          refillCounter := value
          report(Seq("Set refill counter: ", value))
        } otherwise {
          io.mmio.DAT_MISO := refillCounter.asBits.resized
        }
      }
      is(0x01) {
        when(io.mmio.WE) {
          refillBuffer := io.mmio.DAT_MOSI
        }
      }
      is(0x02) {
        when(io.mmio.WE) {
          val data = io.mmio.DAT_MOSI ## refillBuffer
          io.refill.valid := True
          io.refill.payload.addr := refillCounter
          io.refill.payload.insn := data
          refillCounter := refillCounter + 1
          report(Seq("Commit refill: ", refillCounter, " ", data))
        }
      }
      is(0x03) {
        when(io.mmio.WE) {
          io.pcUpdater.valid := True
          io.pcUpdater.payload.pc := io.mmio.DAT_MOSI.asUInt.resized
          io.pcUpdater.payload.flush := True
          report(Seq("Update PC: ", io.mmio.DAT_MOSI.asUInt))
        }
      }
    }
  }
}

object MMIOBusConfig {
  def apply() = WishboneConfig(
    addressWidth = 32,
    dataWidth = 32
  )
}
