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
      Vec(for (i <- 0 until config.numPe) yield new CpuException())
    )
    val dataMemAxi4 = slave(Axi4(DataMemV2Axi4PortConfig()))
    val dataMem = slave(dataMemory.use())
  }
  val peList = (0 until config.numPe).map(i => {
    val pe = new ProcessingElement(config.pe, i)
    val dm = dataMemory.use()
    pe.io.dm.request >> dm.request
    pe.io.dm.response << dm.response
    pe
  })
  val addrMappings = peList.zipWithIndex.map(x =>
    SizeMapping(base = 0x1000 + x._2 * 0x1000, size = 0x1000)
  )
  val mmioWriteDecoder = Axi4WriteOnlyDecoder(
    MMIOBusConfigV2(),
    addrMappings
  )
  mmioWriteDecoder.io.input << io.mmio
  mmioWriteDecoder.io.outputs
    .zip(peList)
    .foreach(x => x._2.io.mmio << x._1)
  val mmioReadDecoder = Axi4ReadOnlyDecoder(
    MMIOBusConfigV2(),
    addrMappings
  )
  mmioReadDecoder.io.input << io.mmio
  mmioReadDecoder.io.outputs
    .zip(peList)
    .foreach(x => x._2.io.mmio << x._1)

  io.excOutput.zip(peList).foreach(x => x._1 := x._2.io.excOutput)

  dataMemory.use().toAxi4WriteOnly() << io.dataMemAxi4
  dataMemory.use().toAxi4ReadOnly() << io.dataMemAxi4
}

object DefaultWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 10
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true
      ),
      dataMemSize = 1024,
      numPe = 2
    )
}
class Wbpf extends CustomWbpf(DefaultWbpfConfig()) {}

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
