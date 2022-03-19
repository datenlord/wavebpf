package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.bus.wishbone._

case class DataMemConfig(
    numWords: Int
)

case class DataMem(c: DataMemConfig) extends Area {
  val data = Mem(Bits(64 bits), c.numWords)
  val users = ArrayBuffer[Wishbone]()
  val dmPort = Wishbone(DataMemWishboneConfig())

  val memBody = Mem(Bits(64 bits), c.numWords)
  memBody.initialContent = (for(i <- 0 until memBody.wordCount) yield BigInt("4343434343434343", 16)).toArray

  val ack = RegInit(False)

  dmPort.ACK := ack
  val addr = dmPort.ADR(log2Up(c.numWords) - 1 downto 0)
  dmPort.DAT_MISO := memBody.readSync(address = addr)
  memBody.write(
    address = addr,
    data = dmPort.DAT_MOSI,
    enable = dmPort.STB && dmPort.WE
  )
  ack := dmPort.STB

  def use(): Wishbone = {
    val port = Wishbone(DataMemWishboneConfig())
    users += port
    port
  }

  Component.current.afterElaboration {
    WishboneArbiter(users, dmPort)
  }
}

case class DataMemPort() extends Bundle {}

object DataMemWishboneConfig {
  def apply() = WishboneConfig(
    addressWidth = 64,
    dataWidth = 64
  )
}
