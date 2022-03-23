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
    println("Size: " + bytes.length)
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      dut.clockDomain.forkStimulus(10)
      loadCode(dut, 0x0, bytes)
      assert(dut.io.excOutput.valid.toBoolean)
      assert(dut.io.excOutput.code.toEnum == CpuExceptionCode.NOT_INIT)
      mmioWrite(dut, 0x03, 0x00)
      mmioEndWrite(dut)

      waitUntil(!dut.io.excOutput.valid.toBoolean || dut.io.excOutput.code.toEnum != CpuExceptionCode.NOT_INIT)
      if(dut.io.excOutput.valid.toBoolean) {
        throw new Exception("Init exception: " + dut.io.excOutput.code.toEnum + " " + dut.io.excOutput.data.toBigInt)
      }

      var cycles = 0L
      dut.clockDomain.onSamplings {
        cycles += 1
      }
      println("Simulation started")

      var shouldStop = false
      var endExc: SpinalEnumElement[CpuExceptionCode.type] = null
      while (!shouldStop) {
        waitUntil(dut.io.excOutput.valid.toBoolean)
        endExc = dut.io.excOutput.code.toEnum
        if (endExc != CpuExceptionCode.PENDING_BRANCH) {
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
