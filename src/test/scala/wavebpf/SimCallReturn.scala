package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite

class SimCallReturnSpec extends AnyFunSuite {
  // tests go here...
  test("SimCallReturn") {
    import SimUtil._
    val config = DefaultWbpfConfig()
    runWithAllBackends(
      new CustomWbpf(config.copy(pe = config.pe.copy(reportCommit = true)))
    ) { dut =>
      initDutForTesting(dut)

      val firstExc = dut.io.excOutput.head

      loadCode(
        dut,
        0,
        0x0,
        Array[Short](
          /*
0000000000000000 entry:
       0:	b7 0a 00 00 00 10 00 00	r10 = 4096
       1:	05 20 01 00 f8 ff ff ff	goto +1 <subfunc>, call, sp:-8

0000000000000010 entry.1:
       2:	95 00 00 00 00 00 00 00	exit

0000000000000018 subfunc:
       3:	bf a0 00 00 00 00 00 00	r0 = r10
       4:	05 10 00 00 08 00 00 00	ret, sp:+8
           */
          // Manually adjusted GOTO immediates!
          0xb7, 0x0a, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00,
          0x05, 0x20, 0x01, 0x00, 0xf8, 0xff, 0xff, 0xff,
          0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          0xbf, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x05, 0x10, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00
        ).map(_.toByte)
      )
      println("Code loaded.")

      assert(firstExc.valid.toBoolean)
      assert(firstExc.code.toEnum == CpuExceptionCode.NOT_INIT)

      mmioWrite(dut, 0x1018, 0x00)
      dut.clockDomain.waitSamplingWhere(!firstExc.valid.toBoolean)
      dut.clockDomain.waitSamplingWhere(firstExc.valid.toBoolean)
      assert(firstExc.code.toEnum == CpuExceptionCode.EXIT)
      assert(firstExc.data.toBigInt == 0x0ff8)
      println("Check passed.")
    }
  }
}
