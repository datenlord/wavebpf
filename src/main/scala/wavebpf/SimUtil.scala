package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object SimUtil {
  def dmReadOnce(dut: CustomWbpf, addr: BigInt): BigInt = {
    dut.io.dataMem.request.valid #= true
    dut.io.dataMem.request.write #= false
    dut.io.dataMem.request.addr #= addr
    dut.io.dataMem.request.width #= MemoryAccessWidth.W8
    dut.io.dataMem.request.precomputedStrbValid #= false
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
      dut: CustomWbpf,
      addr: BigInt,
      word: BigInt,
      width: SpinalEnumElement[MemoryAccessWidth.type] = MemoryAccessWidth.W8
  ) {
    dut.io.dataMem.request.valid #= true
    dut.io.dataMem.request.write #= true
    dut.io.dataMem.request.addr #= addr
    dut.io.dataMem.request.data #= word
    dut.io.dataMem.request.width #= width
    dut.io.dataMem.request.precomputedStrbValid #= false
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

  def dmWriteBytesAxi4(dut: CustomWbpf, addr: Long, data: Seq[Byte]) {
    assert(data.length > 0 && data.length < 256)

    dut.io.dataMemAxi4.aw.valid #= true
    dut.io.dataMemAxi4.aw.payload.addr #= addr
    dut.io.dataMemAxi4.aw.payload.size #= 0x0
    dut.io.dataMemAxi4.aw.payload.len #= data.length - 1
    dut.io.dataMemAxi4.aw.payload.burst #= 1
    dut.clockDomain.waitSamplingWhere(dut.io.dataMemAxi4.aw.ready.toBoolean)
    dut.io.dataMemAxi4.aw.valid #= false

    for ((b_, i) <- data.zipWithIndex) {
      val b = b_.toShort & 0xff
      val shiftBytes = (addr.toInt + i) % 4
      val strb = 1 << shiftBytes
      val shiftedData = BigInt(b) << (shiftBytes * 8)
      dut.io.dataMemAxi4.w.valid #= true
      dut.io.dataMemAxi4.w.payload.data #= shiftedData
      dut.io.dataMemAxi4.w.payload.strb #= strb
      dut.io.dataMemAxi4.w.payload.last #= i + 1 == data.length
      dut.clockDomain.waitSamplingWhere(dut.io.dataMemAxi4.w.ready.toBoolean)
    }
    dut.io.dataMemAxi4.w.valid #= false
    dut.clockDomain.waitSamplingWhere(dut.io.dataMemAxi4.b.valid.toBoolean)
    dut.io.dataMemAxi4.b.ready #= true
    dut.clockDomain.waitSampling()
    dut.io.dataMemAxi4.b.ready #= false
  }

  def initDutForTesting(dut: CustomWbpf) {
    dut.io.dataMem.request.valid #= false
    dut.io.dataMem.response.ready #= false
    dut.io.mmio.ar.valid #= false
    dut.io.mmio.aw.valid #= false
    dut.io.mmio.w.valid #= false
    dut.io.mmio.b.ready #= false
    dut.io.mmio.r.ready #= false
    dut.io.dataMemAxi4.ar.valid #= false
    dut.io.dataMemAxi4.aw.valid #= false
    dut.io.dataMemAxi4.w.valid #= false
    dut.io.dataMemAxi4.b.ready #= false
    dut.io.dataMemAxi4.r.ready #= false
    dut.clockDomain.forkStimulus(10)
    waitUntil(dut.clockDomain.isResetAsserted)
    waitUntil(dut.clockDomain.isResetDeasserted)
  }

  def mmioWrite(dut: CustomWbpf, addr: Long, value: Long) {
    // println("Write MMIO " + addr + " " + value)
    dut.io.mmio.aw.valid #= true
    dut.io.mmio.aw.payload.addr #= addr
    dut.clockDomain.waitSamplingWhere(dut.io.mmio.aw.ready.toBoolean)
    dut.io.mmio.aw.valid #= false

    dut.io.mmio.w.valid #= true
    dut.io.mmio.w.payload.data #= value
    dut.io.mmio.w.payload.last #= true
    dut.clockDomain.waitSamplingWhere(dut.io.mmio.w.ready.toBoolean)
    dut.io.mmio.w.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.mmio.b.valid.toBoolean)
    dut.io.mmio.b.ready #= true
    dut.clockDomain.waitSampling()
    dut.io.mmio.b.ready #= false
  }

  def mmioRead(dut: CustomWbpf, addr: Long): BigInt = {
    // println("Read MMIO " + addr)
    dut.io.mmio.ar.valid #= true
    dut.io.mmio.ar.payload.addr #= addr
    dut.clockDomain.waitSamplingWhere(dut.io.mmio.ar.ready.toBoolean)
    dut.io.mmio.ar.valid #= false

    dut.clockDomain.waitSamplingWhere(dut.io.mmio.r.valid.toBoolean)
    assert(dut.io.mmio.r.last.toBoolean)
    val ret = dut.io.mmio.r.payload.data.toBigInt
    dut.io.mmio.r.ready #= true
    dut.clockDomain.waitSampling()
    dut.io.mmio.r.ready #= false

    ret
  }

  def loadCode(
      dut: CustomWbpf,
      coreIndex: Int,
      baseAddr: Long,
      code: Array[Byte]
  ) {
    mmioWrite(dut, 0x1000 * (coreIndex + 1), baseAddr)

    var upperHalf = false
    var buffer: Int = 0
    var pos: Int = 0

    for (b <- code) {
      buffer |= (b.toInt & 0xff) << (pos * 8)
      pos += 1

      if (pos == 4) {
        mmioWrite(
          dut,
          (if (upperHalf) 0x10 else 0x08) + 0x1000 * (coreIndex + 1),
          buffer.toLong & 0x00000000ffffffffL
        )
        upperHalf = !upperHalf
        pos = 0
        buffer = 0
      }
    }
  }
}
