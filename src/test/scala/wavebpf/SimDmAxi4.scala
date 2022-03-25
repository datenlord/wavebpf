package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite

class SimDmAxi4Spec extends AnyFunSuite {
  // tests go here...
  test("SimDmAxi4") {
    import SimUtil._
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      initDutForTesting(dut)
      dmWriteBytesAxi4(dut, 0x0, Seq(1, 2, 3, 4, 5, 6, 7, 8))
      assert(dmReadOnce(dut, 0x0) == BigInt("0807060504030201", 16))
      dmWriteBytesAxi4(dut, 0x3, Seq(10, 11))
      assert(dmReadOnce(dut, 0x0) == BigInt("0807060b0a030201", 16))
      dmWriteBytesAxi4(dut, 0x7, Seq(42, 43, 44, 45, 46, 47, 48, 49, 50))
      assert(dmReadOnce(dut, 0x0) == BigInt("2a07060b0a030201", 16))
      assert(dmReadOnce(dut, 0x8) == BigInt("3231302f2e2d2c2b", 16))
    }
  }
}
