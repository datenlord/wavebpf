package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

//MyTopLevel's testbench
object WbpfSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      dut.clockDomain.forkStimulus(10)

      /*
      void memAdd() {
        long a = *(long *)0x10;
        long b = *(long *)0x18;
       *(long *)0x20 = a + b;
        while(1);
      }
       */
      loadCode(
        dut,
        0x0,
        Array(
          0xb7, 0x01, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, // r1 = 0x10
          0x79, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, // r1 = *(u64 *)(r1 + 0)
          0xb7, 0x02, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, // r2 = 0x18
          0x79, 0x22, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, // r2 = *(u64 *)(r2 + 0)
          0x0f, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // r2 += r1
          0xb7, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, // r1 = 0x20
          0x7b, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, // *(u64 *)(r1 + 0) = r2
          0x00, 0x00, 0x00, 0x00, 0x00, 0x11, 0x00, 0x11 // BAD INSTRUCTION
        )
      )
      println("Code loaded.")

      assert(dut.io.excOutput.valid.toBoolean)
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.NOT_INIT)

      mmioWrite(dut, 0x03, 0x00)
      mmioEndWrite(dut)
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.NOT_INIT)
      dut.clockDomain.waitSampling()
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.NOT_INIT)
      dut.clockDomain.waitSampling()
      assert(dut.io.excOutput.valid.toBoolean == false)
      while(!dut.io.excOutput.valid.toBoolean) {
        println("*** CLOCK CYCLE ***")
        dut.clockDomain.waitSampling()
      }
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.BAD_INSTRUCTION)
      assert(dut.io.excOutput.data.toBigInt == BigInt("1100110000000000", 16))

      println("Code execution completed.")
      for(i <- 0 to 2) {
        println("*** CLOCK CYCLE ***")
        dut.clockDomain.waitSampling()
      }

      println("Running check.")
      assert(dmReadOnce(dut, 0x20 / 8) == BigInt("8686868686868686", 16))
      println("Check passed.")
    }
  }

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

  def loadCode(dut: Wbpf, baseAddr: Long, code: Array[Short]) {
    mmioWrite(dut, 0x00, baseAddr)
    mmioEndWrite(dut)

    var upperHalf = false
    var buffer: Int = 0
    var pos: Int = 0

    for (b <- code) {
      buffer |= b.toInt << (pos * 8)
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
