package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite

class SimDmAxi4Spec extends AnyFunSuite {
  // tests go here...
  test("SimDmAxi4") {
    import SimUtil._
    runWithAllConfig { dut =>
      initDutForTesting(dut)
      dmWriteBytesAxi4(dut, 0x0, Seq(1, 2, 3, 4, 5, 6, 7, 8))
      assert(dmReadOnce(dut, 0x0) == BigInt("0807060504030201", 16))
      dmWriteBytesAxi4(dut, 0x3, Seq(10, 11))
      assert(dmReadOnce(dut, 0x0) == BigInt("0807060b0a030201", 16))
      dmWriteBytesAxi4(dut, 0x7, Seq(42, 43, 44, 45, 46, 47, 48, 49, 50))
      assert(dmReadOnce(dut, 0x0) == BigInt("2a07060b0a030201", 16))
      assert(dmReadOnce(dut, 0x8) == BigInt("3231302f2e2d2c2b", 16))
      assert(
        dmBurstReadAxi4(dut, 0x8, 2, 1).toSeq == Seq(BigInt("2e2d2c2b", 16))
      )
      // XXX: Unaligned reads are not supported
      /*
      assert(
        dmBurstReadAxi4(dut, 0x9, 2, 1).toSeq == Seq(BigInt("2f2e2d2c", 16))
      )
       */
      assert(
        dmBurstReadAxi4(dut, 0xc, 2, 1).toSeq == Seq(BigInt("3231302f", 16))
      )
      assert(
        dmBurstReadAxi4(dut, 0x7, 0, 6).toSeq == Seq(0x2a, 0x2b, 0x2c, 0x2d,
          0x2e, 0x2f)
      )
      assert(
        dmBurstReadAxi4(dut, 0x0, 2, 4).toSeq == Seq(
          BigInt("0a030201", 16),
          BigInt("2a07060b", 16),
          BigInt("2e2d2c2b", 16),
          BigInt("3231302f", 16)
        )
      )
    }
  }
}
