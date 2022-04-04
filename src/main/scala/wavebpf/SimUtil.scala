package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object SimUtil {
  def runWithAllBackends[T <: Component](
      rtl: => T
  )(body: T => Unit): Unit = {
    // iverilog is too slow
    // SimConfig.withIVerilog.doSim(rtl)(body)

    SimConfig.withVerilator.doSim(rtl)(body)
    //SimConfig.withVerilator.doSim(rtl = rtl, name = "test", seed = 42)(body)
  }

  def runWithAllConfig(body: CustomWbpf => Unit): Unit = {
    val config: Seq[(String, WbpfConfig)] = Seq(
      ("default", DefaultWbpfConfig()),
      ("noMemBypass", NoMemBypassWbpfConfig()),
      ("withBtb", WithBtbWbpfConfig())
    )
    for ((name, c) <- config) {
      println("Running with config: " + name)
      runWithAllBackends(
        new CustomWbpf(c.copy(pe = c.pe.copy(reportCommit = false)))
      )(body)
    }
  }

  def dmReadOnce(
      dut: CustomWbpf,
      addr: BigInt,
      width: SpinalEnumElement[MemoryAccessWidth.type] = MemoryAccessWidth.W8
  ): BigInt = {
    dut.io.dataMem.request.valid #= true
    dut.io.dataMem.request.write #= false
    dut.io.dataMem.request.addr #= addr
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
    assert(data.length > 0 && data.length <= 256)

    dut.io.dataMemAxi4.aw.valid #= true
    dut.io.dataMemAxi4.aw.payload.addr #= addr
    dut.io.dataMemAxi4.aw.payload.size #= 0x0
    dut.io.dataMemAxi4.aw.payload.len #= data.length - 1
    dut.io.dataMemAxi4.aw.payload.burst #= 1
    dut.clockDomain.waitSamplingWhere(dut.io.dataMemAxi4.aw.ready.toBoolean)
    dut.io.dataMemAxi4.aw.valid #= false

    for ((b_, i) <- data.zipWithIndex) {
      val b = b_.toShort & 0xff
      val shiftBytes =
        (addr.toInt + i) % (dut.io.dataMemAxi4.config.dataWidth / 8)
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

  def dmBurstReadAxi4(
      dut: CustomWbpf,
      addr: Long,
      size: Int,
      len: Int
  ): Seq[BigInt] = {
    assert(size == 0 || size == 1 || size == 2)
    assert(len > 0 && len <= 256)

    dut.io.dataMemAxi4.ar.valid #= true
    dut.io.dataMemAxi4.ar.payload.addr #= addr
    dut.io.dataMemAxi4.ar.payload.size #= size
    dut.io.dataMemAxi4.ar.payload.len #= len - 1
    dut.io.dataMemAxi4.ar.payload.burst #= 1
    dut.clockDomain.waitSamplingWhere(dut.io.dataMemAxi4.ar.ready.toBoolean)
    dut.io.dataMemAxi4.ar.valid #= false

    val ret = new Array[BigInt](len)
    for (i <- 0 until len) {
      val curAddr = addr + (i.toLong * scala.math.pow(2, size))
      dut.clockDomain.waitSamplingWhere(dut.io.dataMemAxi4.r.valid.toBoolean)
      if (i + 1 == len) {
        assert(dut.io.dataMemAxi4.r.last.toBoolean)
      }
      val value = dut.io.dataMemAxi4.r.payload.data.toBigInt
      val shiftedValue =
        value >> ((curAddr % (dut.io.dataMemAxi4.config.dataWidth / 8)) * 8).toInt
      val mask =
        if (size == 0) 0xffL else if (size == 1) 0xffffL else 0xffffffffL
      ret.update(i, shiftedValue & mask)
      dut.io.dataMemAxi4.r.ready #= true
      dut.clockDomain.waitSampling()
      dut.io.dataMemAxi4.r.ready #= false
    }

    ret
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
    val ret = dut.io.mmio.r.payload.data.toBigInt
    dut.io.mmio.r.ready #= true
    dut.clockDomain.waitSampling()
    dut.io.mmio.r.ready #= false

    ret
  }

  def resetPerfCounters(dut: CustomWbpf, peIndex: Int): Unit = {
    mmioWrite(dut, 0x1000 * (peIndex + 1) + 0x10 * 4, 0)
  }

  def readPerfCounters(dut: CustomWbpf, peIndex: Int): (BigInt, BigInt) = {
    val cycles =
      (mmioRead(dut, 0x1000 * (peIndex + 1) + 0x11 * 4) << 32) | mmioRead(
        dut,
        0x1000 * (peIndex + 1) + 0x10 * 4
      )
    val commits =
      (mmioRead(dut, 0x1000 * (peIndex + 1) + 0x13 * 4) << 32) | mmioRead(
        dut,
        0x1000 * (peIndex + 1) + 0x12 * 4
      )
    (cycles, commits)
  }

  def printPerfCounters(dut: CustomWbpf, peIndex: Int): Unit = {
    val (cycles, commits) = readPerfCounters(dut, peIndex)
    println(
      "perfcounters pe@" + peIndex + ": cycles=" + cycles + " commits=" + commits + " utilization=" + (commits.toDouble / cycles.toDouble)
    )
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
