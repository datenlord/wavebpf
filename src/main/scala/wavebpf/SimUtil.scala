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

  def dmWriteOnce(
      dut: Wbpf,
      addr: BigInt,
      word: BigInt,
      width: SpinalEnumElement[MemoryAccessWidth.type] = MemoryAccessWidth.W8
  ) {
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

  def initDutForTesting(dut: Wbpf) {
    dut.io.dataMem.request.valid #= false
    dut.io.dataMem.response.ready #= false
    dut.io.mmio.ar.valid #= false
    dut.io.mmio.aw.valid #= false
    dut.io.mmio.w.valid #= false
    dut.io.mmio.b.ready #= false
    dut.io.mmio.r.ready #= false
    dut.clockDomain.forkStimulus(10)
    waitUntil(dut.clockDomain.isResetAsserted)
    waitUntil(dut.clockDomain.isResetDeasserted)
  }

  def mmioWrite(dut: Wbpf, addr: Long, value: Long) {
    dut.io.mmio.aw.valid #= true
    dut.io.mmio.aw.payload.addr #= addr * 8
    waitUntil(dut.io.mmio.aw.ready.toBoolean)
    dut.clockDomain.waitSampling()
    dut.io.mmio.aw.valid #= false

    dut.io.mmio.w.valid #= true
    dut.io.mmio.w.payload.data #= value
    dut.io.mmio.w.payload.last #= true
    waitUntil(dut.io.mmio.w.ready.toBoolean)
    dut.clockDomain.waitSampling()
    dut.io.mmio.w.valid #= false

    waitUntil(dut.io.mmio.b.valid.toBoolean)
    dut.io.mmio.b.ready #= true
    dut.clockDomain.waitSampling()
    dut.io.mmio.b.ready #= false
  }

  def loadCode(dut: Wbpf, baseAddr: Long, code: Array[Byte]) {
    mmioWrite(dut, 0x00, baseAddr)

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
  }
}
