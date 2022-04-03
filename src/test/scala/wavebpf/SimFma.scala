package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

class SimFmaSpec extends AnyFunSuite {
  // tests go here...
  test("SimFma") {
    import SimUtil._
    runWithAllConfig { dut =>
      initDutForTesting(dut)

      val firstExc = dut.io.excOutput.head

      loadCode(
        dut,
        0,
        0x0,
        Array[Short](
          0xbc, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x67, 0x05, 0x00,
          0x00, 0x20, 0x00, 0x00, 0x00, 0xc7, 0x05, 0x00, 0x00, 0x20, 0x00,
          0x00, 0x00, 0xb7, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x6d,
          0x50, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x67, 0x04, 0x00, 0x00,
          0x20, 0x00, 0x00, 0x00, 0x77, 0x04, 0x00, 0x00, 0x20, 0x00, 0x00,
          0x00, 0x61, 0x15, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x61, 0x20,
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2c, 0x50, 0x00, 0x00, 0x00,
          0x00, 0x00, 0x00, 0x61, 0x35, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x0c, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x63, 0x53, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x01, 0x00, 0x00, 0x04, 0x00,
          0x00, 0x00, 0x07, 0x02, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x07,
          0x03, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x07, 0x04, 0x00, 0x00,
          0xff, 0xff, 0xff, 0xff, 0x55, 0x04, 0xf5, 0xff, 0x00, 0x00, 0x00,
          0x00, 0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ).map(_.toByte)
      )
      println("Code loaded.")

      val n = 1000
      val vecA: Seq[Int] = (0 until n).map((x) => Random.nextInt(100000000) + 1)
      val vecB: Seq[Int] = (0 until n).map((x) => Random.nextInt(100000000) + 1)
      val vecC: Seq[Int] = (0 until n).map((x) => Random.nextInt(100000000) + 1)
      var addr = 0x100L

      // R1
      mmioWrite(dut, 0x1008, addr)
      mmioWrite(dut, 0x108c, 0)
      for (x <- vecA) {
        dmWriteOnce(dut, addr, x, MemoryAccessWidth.W4)
        addr += 4
      }

      // R2
      mmioWrite(dut, 0x1008, addr)
      mmioWrite(dut, 0x1094, 0)
      for (x <- vecB) {
        dmWriteOnce(dut, addr, x, MemoryAccessWidth.W4)
        addr += 4
      }

      // R3
      val outAddr = addr
      mmioWrite(dut, 0x1008, addr)
      mmioWrite(dut, 0x109c, 0)
      for (x <- vecC) {
        dmWriteOnce(dut, addr, x, MemoryAccessWidth.W4)
        addr += 4
      }

      // R4
      mmioWrite(dut, 0x1008, n)
      mmioWrite(dut, 0x10a4, 0)

      assert(firstExc.valid.toBoolean)
      assert(firstExc.code.toEnum == CpuExceptionCode.NOT_INIT)

      mmioWrite(dut, 0x1018, 0x00)
      resetPerfCounters(dut, 0)
      dut.clockDomain.waitSamplingWhere(!firstExc.valid.toBoolean)
      dut.clockDomain.waitSamplingWhere(firstExc.valid.toBoolean)
      assert(firstExc.code.toEnum == CpuExceptionCode.EXIT)
      printPerfCounters(dut, 0)

      println("Code execution completed. Running check.")
      for (i <- 0 until n) {
        val thisAddr = outAddr + i * 4
        val data =
          (dmReadOnce(dut, thisAddr, MemoryAccessWidth.W4) & 0xffffffffL).toInt
        if (data != vecA(i) * vecB(i) + vecC(i)) {
          throw new Exception(
            s"Error at $i: $data != ${vecA(i)} * ${vecB(i)} + ${vecC(i)}"
          )
        }
      }
      println("Check passed.")
    }
  }
}
