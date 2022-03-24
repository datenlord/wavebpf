package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random
import java.nio.file.{Files, Paths}

//MyTopLevel's testbench
object WbpfSim {
  import SimUtil._
  def main(args: Array[String]) {
    val bytes = Files.readAllBytes(Paths.get(args(0)))
    if (bytes.length % 8 != 0) {
      throw new Exception("Input file must be multiple of 8 bytes")
    }
    println("Code size: " + bytes.length)

    var dmImage = new Array[Byte](0)
    if (args.length >= 2) {
      dmImage = Files.readAllBytes(Paths.get(args(1)))
      println("DM size: " + dmImage.length)
    }
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      initDutForTesting(dut)
      loadCode(dut, 0x0, bytes)

      for ((b, i) <- dmImage.zipWithIndex) {
        dmWriteOnce(dut, i, b, MemoryAccessWidth.W1)
      }

      assert(dut.io.excOutput.valid.toBoolean)
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.NOT_INIT)
      mmioWrite(dut, 0x03, 0x00)

      waitUntil(
        !dut.io.excOutput.valid.toBoolean || dut.io.excOutput.code.toEnum != CpuExceptionCode.NOT_INIT
      )
      if (dut.io.excOutput.valid.toBoolean) {
        throw new Exception(
          "Init exception: " + dut.io.excOutput.code.toEnum + " " + dut.io.excOutput.data.toBigInt
        )
      }

      var cycles = 0L
      dut.clockDomain.onSamplings {
        cycles += 1
      }
      println("Simulation started")

      var shouldStop = false
      var endExc: SpinalEnumElement[CpuExceptionCode.type] = null
      while (!shouldStop) {
        dut.clockDomain.waitSampling()
        if (
          dut.io.excOutput.valid.toBoolean && dut.io.excOutput.code.toEnum != CpuExceptionCode.PENDING_BRANCH
        ) {
          endExc = dut.io.excOutput.code.toEnum
          shouldStop = true
        }
        if (cycles >= 10000) {
          shouldStop = true
        }
      }
      println("Simulation ended after " + cycles + " cycles")
      if (endExc != CpuExceptionCode.EXIT) {
        throw new Exception("CPU exception: " + endExc)
      }
    }
  }
}
