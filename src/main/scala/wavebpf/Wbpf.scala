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
    numPe: Int,
    downsizeDataMemPort: Boolean
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
    val dataMemAxi4 = slave(
      Axi4(
        if (config.downsizeDataMemPort) DataMemV2Axi4DownsizedPortConfig()
        else DataMemV2Axi4PortConfig()
      )
    )
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

  if (config.downsizeDataMemPort) {
    val dmUpsizer = Axi4Upsizer(io.dataMemAxi4.config, dataMemAxi4.config, 4)
    io.dataMemAxi4 >> dmUpsizer.io.input
    dmUpsizer.io.output >> dataMemAxi4
  } else {
    io.dataMemAxi4 >> dataMemAxi4
  }

}

object DefaultWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11,
          useBtb = true,
          btbSize = 8
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = false
      ),
      dataMemSize = 32768,
      numPe = 4,
      downsizeDataMemPort = false
    )
}
class Wbpf extends CustomWbpf(DefaultWbpfConfig()) {}

class WbpfSynth extends Component {
  val wbpf = new Wbpf()

  val io = new Bundle {
    val mmio = slave(AxiLite4(MMIOBusConfigV2()))
    val dataMemAxi4 = slave(Axi4(wbpf.io.dataMemAxi4.config))
    val excInterrupt = out Bool ()
  }
  wbpf.io.dataMem.request.setIdle()
  wbpf.io.dataMem.response.setBlocked()
  wbpf.io.mmio << io.mmio
  wbpf.io.dataMemAxi4 << io.dataMemAxi4
  io.excInterrupt := wbpf.io.excInterrupt
}

class WbpfSynthHighFreq extends Component {
  val io = new Bundle {
    val mmio = slave(AxiLite4(MMIOBusConfigV2()))
    val dataMemAxi4 = slave(Axi4(DataMemV2Axi4DownsizedPortConfig()))
    val excInterrupt = out Bool ()
    val logic_clk = out Bool()
  }

  val topDomain = ClockDomain.current
  val dividedClock = Reg(Bool()) init(False)
  dividedClock := !dividedClock

  val dividedReset = Reg(Bool()) init(True)
  when(dividedClock) {
    dividedReset := False
  }

  val logicDomain = ClockDomain.internal("logic")
  logicDomain.clock := dividedClock
  logicDomain.reset := dividedReset

  io.logic_clk := dividedClock

  val logicArea = new ClockingArea(logicDomain) {
    val wbpf = new WbpfSynth()
    assert(wbpf.io.dataMemAxi4.config.dataWidth == 64)
  }
  logicArea.wbpf.io.mmio.ar << io.mmio.ar.queue(4, topDomain, logicDomain)
  logicArea.wbpf.io.mmio.aw << io.mmio.aw.queue(4, topDomain, logicDomain)
  logicArea.wbpf.io.mmio.w << io.mmio.w.queue(4, topDomain, logicDomain)
  io.mmio.r << logicArea.wbpf.io.mmio.r.queue(4, logicDomain, topDomain)
  io.mmio.b << logicArea.wbpf.io.mmio.b.queue(4, logicDomain, topDomain)

  io.excInterrupt := logicArea.wbpf.io.excInterrupt

  val dmUpsizer =
    Axi4Upsizer(io.dataMemAxi4.config, logicArea.wbpf.io.dataMemAxi4.config, 4)
  io.dataMemAxi4 >> dmUpsizer.io.input
  val dmCC = Axi4CC(
    axiConfig = dmUpsizer.io.output.config,
    inputCd = topDomain,
    outputCd = logicDomain,
    arFifoSize = 4,
    awFifoSize = 4,
    rFifoSize = 4,
    wFifoSize = 4,
    bFifoSize = 4
  )
  dmUpsizer.io.output >> dmCC.io.input
  dmCC.io.output >> logicArea.wbpf.io.dataMemAxi4
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

object WbpfVerilogSyncResetHighFreq {
  def main(args: Array[String]) {
    SyncResetSpinalConfig.generateVerilog(new WbpfSynthHighFreq)
  }
}
