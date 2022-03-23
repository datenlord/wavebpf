package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object SimUtil {
  def dmReadOnce(dut: Wbpf, addr: BigInt): BigInt = {
    dut.io.dataMem.CYC #= true
    dut.io.dataMem.STB #= true
    dut.io.dataMem.ADR #= addr
    dut.io.dataMem.WE #= false
    waitUntil(dut.io.dataMem.ACK.toBoolean)
    dut.io.dataMem.DAT_MISO.toBigInt
  }

  def mmioWrite(dut: Wbpf, addr: Long, value: Long) {
    dut.io.mmio.CYC #= true
    dut.io.mmio.STB #= true
    dut.io.mmio.ADR #= addr
    dut.io.mmio.DAT_MOSI #= value
    dut.io.mmio.WE #= true
    dut.clockDomain.waitSampling()
    assert(dut.io.mmio.ACK.toBoolean == true)
  }

  def mmioEndWrite(dut: Wbpf) {
    dut.io.mmio.CYC #= false
    dut.io.mmio.STB #= false
    dut.clockDomain.waitSampling()
    assert(dut.io.mmio.ACK.toBoolean == false)
  }

  def loadCode(dut: Wbpf, baseAddr: Long, code: Array[Byte]) {
    mmioWrite(dut, 0x00, baseAddr)
    mmioEndWrite(dut)

    var upperHalf = false
    var buffer: Int = 0
    var pos: Int = 0

    for (b <- code) {
      buffer |= (b.toInt & 0xff) << (pos * 8)
      pos += 1

      if (pos == 4) {
        mmioWrite(
          dut,
          if (upperHalf) 0x02 else 0x01,
          buffer.toLong & 0x00000000ffffffffL
        )
        upperHalf = !upperHalf
        pos = 0
        buffer = 0
      }
    }

    mmioEndWrite(dut)
  }
}
