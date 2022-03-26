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
    val dataMemAxi4 = slave(Axi4(DataMemV2Axi4DownsizedPortConfig()))
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

  val dataMemAxi4 = Axi4(DataMemV2Axi4PortConfig())

  dataMemory.use().toAxi4WriteOnly() << dataMemAxi4
  dataMemory.use().toAxi4ReadOnly() << dataMemAxi4

  val dmUpsizer = Axi4Upsizer(io.dataMemAxi4.config, dataMemAxi4.config, 4)
  io.dataMemAxi4 >> dmUpsizer.io.input
  dmUpsizer.io.output >> dataMemAxi4
}

object DefaultWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true
      ),
      dataMemSize = 8192,
      numPe = 4
    )
}
class Wbpf extends CustomWbpf(DefaultWbpfConfig()) {}

class WbpfSynth extends Component {
  val io = new Bundle {
    val mmio = slave(Axi4(MMIOBusConfigV2()))
    val dataMemAxi4 = slave(Axi4(DataMemV2Axi4DownsizedPortConfig()))
  }
  val wbpf = new Wbpf()
  wbpf.io.dataMem.request.setIdle()
  wbpf.io.dataMem.response.setBlocked()
  wbpf.io.mmio << io.mmio
  wbpf.io.dataMemAxi4 << io.dataMemAxi4
}

object WbpfVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new WbpfSynth)
  }
}

object SyncResetSpinalConfig
    extends SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
    )

object WbpfVerilogSyncReset {
  def main(args: Array[String]) {
    SyncResetSpinalConfig.generateVerilog(new WbpfSynth)
  }
}
