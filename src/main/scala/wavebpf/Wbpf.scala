package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axilite._
import wavebpf.util._

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
    val mmio = slave(AxiLite4(MMIOBusConfigV2()))
    val excOutput = out(
      Vec(for (i <- 0 until config.numPe) yield new CpuException())
    )
    val dataMemAxi4 = slave(Axi4(DataMemV2Axi4DownsizedPortConfig()))
    val dataMem = slave(dataMemory.use())
    val excInterrupt = out Bool ()
  }
  val peList = (0 until config.numPe).map(i => {
    val pe = new ProcessingElement(
      config.pe,
      PeContextData(coreIndex = i, numPe = config.numPe)
    )
    val dm = dataMemory.use()
    pe.io.dm.request >> dm.request
    pe.io.dm.response << dm.response
    pe
  })
  val addrMappings = peList.zipWithIndex.map(x =>
    SizeMapping(base = 0x1000 + x._2 * 0x1000, size = 0x1000)
  )
  io.excInterrupt := peList.map(_.io.excInterrupt).reduce((a, b) => a || b)
  val mmioWriteDecoder = AxiLite4WriteOnlyDecoder(
    MMIOBusConfigV2(),
    addrMappings
  )
  mmioWriteDecoder.io.input.aw << io.mmio.aw.translateWith(
    WbpfUtil.truncateAxiLite4Address(io.mmio.aw.payload, 15 downto 0)
  )
  mmioWriteDecoder.io.input.w << io.mmio.w
  mmioWriteDecoder.io.input.b >> io.mmio.b
  mmioWriteDecoder.io.outputs
    .zip(peList)
    .foreach(x => {
      x._2.io.mmio.aw << x._1.aw
      x._2.io.mmio.w << x._1.w
      x._2.io.mmio.b >> x._1.b
    })
  val mmioReadDecoder = AxiLite4ReadOnlyDecoder(
    MMIOBusConfigV2(),
    addrMappings
  )
  mmioReadDecoder.io.input.ar << io.mmio.ar.translateWith(
    WbpfUtil.truncateAxiLite4Address(io.mmio.ar.payload, 15 downto 0)
  )
  mmioReadDecoder.io.input.r >> io.mmio.r

  mmioReadDecoder.io.outputs
    .zip(peList)
    .foreach(x => {
      x._2.io.mmio.ar << x._1.ar
      x._2.io.mmio.r >> x._1.r
    })

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
          addrWidth = 11,
          btbSize = 8
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = false
      ),
      dataMemSize = 8192,
      numPe = 4
    )
}
class Wbpf extends CustomWbpf(DefaultWbpfConfig()) {}

class WbpfSynth extends Component {
  val io = new Bundle {
    val mmio = slave(AxiLite4(MMIOBusConfigV2()))
    val dataMemAxi4 = slave(Axi4(DataMemV2Axi4DownsizedPortConfig()))
    val excInterrupt = out Bool ()
  }
  val wbpf = new Wbpf()
  wbpf.io.dataMem.request.setIdle()
  wbpf.io.dataMem.response.setBlocked()
  wbpf.io.mmio << io.mmio
  wbpf.io.dataMemAxi4 << io.dataMemAxi4
  io.excInterrupt := wbpf.io.excInterrupt
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
