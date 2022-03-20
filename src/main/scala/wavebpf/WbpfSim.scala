package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

//MyTopLevel's testbench
object WbpfSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      dut.clockDomain.fallingEdge()
      sleep(1)
      dut.clockDomain.assertReset()
      dut.clockDomain.risingEdge()
      sleep(1)
      dut.clockDomain.deassertReset()
      dut.clockDomain.fallingEdge()
      sleep(1)

      /*
      void memAdd() {
        long a = *(long *)0x10;
        long b = *(long *)0x14;
       *(long *)0x18 = a + b;
        while(1);
      }
       */
      loadCode(
        dut,
        0x0,
        Array(
          0xb7, 0x01, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, // r1 = 16
          0x79, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, // r1 = *(u64 *)(r1 + 0)
          0xb7, 0x02, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, // r2 = 20
          0x79, 0x22, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, // r2 = *(u64 *)(r2 + 0)
          0x0f, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // r2 += r1
          0xb7, 0x01, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, // r1 = 24
          0x7b, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, // *(u64 *)(r1 + 0) = r2
          0x05, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00 // goto -1 <LBB5_1>
        )
      )
      println("Code loaded.")

      assert(dut.io.excOutput.valid.toBoolean)
      println(dut.io.excOutput.code.toEnum)
      println(dut.io.excOutput.data.toBigInt)
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.NOT_INIT)
    }
  }

  def loadCode(dut: Wbpf, baseAddr: Long, code: Array[Short]) {
    dut.io.mmio.CYC #= true
    dut.io.mmio.STB #= true
    dut.io.mmio.ADR #= 0x00
    dut.io.mmio.DAT_MOSI #= baseAddr
    dut.clockDomain.risingEdge()
    sleep(1)
    assert(dut.io.mmio.ACK.toBoolean == true)
    dut.clockDomain.fallingEdge()
    sleep(1)

    dut.io.mmio.CYC #= false
    dut.io.mmio.STB #= false
    dut.clockDomain.risingEdge()
    sleep(1)
    assert(dut.io.mmio.ACK.toBoolean == false)
    dut.clockDomain.fallingEdge()
    sleep(1)

    var upperHalf = false
    var buffer: Int = 0
    var pos: Int = 0

    for (b <- code) {
      buffer |= b.toInt << (pos * 8)
      pos += 1

      if (pos == 4) {
        dut.io.mmio.CYC #= true
        dut.io.mmio.STB #= true
        dut.io.mmio.ADR #= (if (upperHalf) 0x02 else 0x01)
        dut.io.mmio.DAT_MOSI #= buffer.toLong & 0x00000000ffffffffL
        dut.clockDomain.risingEdge()
        sleep(1)
        assert(dut.io.mmio.ACK.toBoolean == true)
        dut.clockDomain.fallingEdge()
        sleep(1)
        upperHalf = !upperHalf
        buffer = 0
        pos = 0
      }
    }

    dut.io.mmio.CYC #= false
    dut.io.mmio.STB #= false
    dut.clockDomain.risingEdge()
    sleep(1)
    assert(dut.io.mmio.ACK.toBoolean == false)
    dut.clockDomain.fallingEdge()
    sleep(1)
  }
}
