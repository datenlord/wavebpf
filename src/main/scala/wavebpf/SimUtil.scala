package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object SimUtil {
  def dmReadOnce(dut: Wbpf, addr: BigInt): BigInt = {
    dut.io.dataMem.request.valid #= true
    dut.io.dataMem.request.write #= false
    dut.io.dataMem.request.addr #= addr
    dut.io.dataMem.request.width #= MemoryAccessWidth.W8
    dut.io.dataMem.response.ready #= true

    dut.clockDomain.waitSampling()

    while (!dut.io.dataMem.request.ready.toBoolean) {
      dut.clockDomain.waitSampling()
    }
    dut.io.dataMem.request.valid #= false

    while (!dut.io.dataMem.response.valid.toBoolean) {
      dut.clockDomain.waitSampling()
    }
    dut.io.dataMem.response.payload.data.toBigInt
  }

  def dmWriteOnce(dut: Wbpf, addr: BigInt, word: BigInt, width: SpinalEnumElement[MemoryAccessWidth.type] = MemoryAccessWidth.W8) {
    dut.io.dataMem.request.valid #= true
    dut.io.dataMem.request.write #= true
    dut.io.dataMem.request.addr #= addr
    dut.io.dataMem.request.data #= word
    dut.io.dataMem.request.width #= width
    dut.io.dataMem.response.ready #= true

    dut.clockDomain.waitSampling()

    while (!dut.io.dataMem.request.ready.toBoolean) {
      dut.clockDomain.waitSampling()
    }
    dut.io.dataMem.request.valid #= false

    while (!dut.io.dataMem.response.valid.toBoolean) {
      dut.clockDomain.waitSampling()
    }
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
