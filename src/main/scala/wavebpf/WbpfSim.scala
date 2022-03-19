package wavebpf

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

//MyTopLevel's testbench
object WbpfSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new Wbpf) { dut =>
      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.io.mmio.CYC #= true
      dut.io.mmio.STB #= true
      dut.io.mmio.ADR #= 0x00
      dut.io.mmio.DAT_MOSI #= 0x00
      dut.clockDomain.waitRisingEdge()
      assert(dut.io.mmio.ACK.toBoolean == true)
      
      dut.io.mmio.CYC #= false
      dut.io.mmio.STB #= false
      dut.clockDomain.waitRisingEdge()
      assert(dut.io.mmio.ACK.toBoolean == false)
    }
  }
}
