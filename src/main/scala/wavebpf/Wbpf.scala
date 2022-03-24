package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping

case class WbpfConfig(
    pe: PeConfig,
    dataMemSize: Int,
    numPe: Int
)

class CustomWbpf(config: WbpfConfig) extends Component {
  val dataMemory = new DataMemV2(
    c = DataMemConfig(
      numWords = config.dataMemSize
    )
  )
  val io = new Bundle {
    val mmio = slave(Axi4(MMIOBusConfigV2()))
    val excOutput = out(
      Vec(for (i <- 0 until config.numPe) yield CpuException())
    )
    val dataMem = slave(dataMemory.use())
  }
  val peList = (0 until config.numPe).map(i => {
    val pe = new ProcessingElement(config.pe)
    val dm = dataMemory.use()
    pe.io.dm.request >> dm.request
    pe.io.dm.response << dm.response
    pe
  })
  val mmioDecoder = Axi4WriteOnlyDecoder(
    MMIOBusConfigV2(),
    peList.zipWithIndex.map(x =>
      SizeMapping(base = 0x1000 + x._2 * 0x1000, size = 0x1000)
    )
  )
  io.mmio.ar.setBlocked()
  io.mmio.r.setIdle()
  mmioDecoder.io.input << io.mmio
  mmioDecoder.io.outputs
    .zip(peList)
    .foreach(x => x._2.io.mmio << x._1.toAxi4())
  io.excOutput.zip(peList).foreach(x => x._1 := x._2.io.excOutput)
}

class Wbpf
    extends CustomWbpf(
      WbpfConfig(
        pe = PeConfig(
          insnBuffer = InsnBufferConfig(
            addrWidth = 10
          ),
          regFetch = RegfetchConfig()
        ),
        dataMemSize = 1024,
        numPe = 2
      )
    ) {}

object WbpfVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Wbpf)
  }
}

object SyncResetSpinalConfig
    extends SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
    )

object WbpfVerilogSyncReset {
  def main(args: Array[String]) {
    SyncResetSpinalConfig.generateVerilog(new Wbpf)
  }
}
