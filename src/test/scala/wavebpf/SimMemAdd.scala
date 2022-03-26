package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite

class SimMemAddSpec extends AnyFunSuite {
  // tests go here...
  test("SimMemAdd") {
    import SimUtil._
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      initDutForTesting(dut)

      val firstExc = dut.io.excOutput.head

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
        0,
        0x0,
        Array[Short](
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
          0x95, 0x00, 0x00, 0x00, 0x00, 0x11, 0x00, 0x11 // EXIT
        ).map(_.toByte)
      )
      println("Code loaded.")

      dmWriteOnce(dut, 0x10, 0x4340)
      dmWriteOnce(dut, 0x18, 0x1111)

      assert(firstExc.valid.toBoolean)
      assert(firstExc.code.toEnum == CpuExceptionCode.NOT_INIT)

      assert(mmioRead(dut, 0x1018) == 0x0) // pc
      assert(mmioRead(dut, 0x1020) == 1) // code

      assert(!dut.io.excInterrupt.toBoolean)

      mmioWrite(dut, 0x1018, 0x00)
      dut.clockDomain.waitSamplingWhere(!firstExc.valid.toBoolean)
      while (!firstExc.valid.toBoolean) {
        println("*** CLOCK CYCLE ***")
        dut.clockDomain.waitSampling()
      }
      assert(firstExc.code.toEnum == CpuExceptionCode.EXIT)

      assert(dut.io.excInterrupt.toBoolean)
      mmioWrite(dut, 0x1020, 0x0)
      assert(!dut.io.excInterrupt.toBoolean)

      assert(mmioRead(dut, 0x1018) == 0x38) // pc
      assert(mmioRead(dut, 0x1020) == 5) // code

      // Test STOP command
      mmioWrite(dut, 0x1004, 0x00)
      while (mmioRead(dut, 0x1020) != 7) {
        println("Wait for stop...")
        dut.clockDomain.waitSampling()
      }

      println("Code execution completed.")
      for (i <- 0 to 2) {
        println("*** CLOCK CYCLE ***")
        dut.clockDomain.waitSampling()
      }

      println("Running check.")
      assert(dmReadOnce(dut, 0x20) == BigInt("5451", 16))
      println("Check passed.")
    }
  }
}
