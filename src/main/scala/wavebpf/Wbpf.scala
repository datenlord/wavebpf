package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._

case class WbpfConfig(
    insnBuffer: InsnBufferConfig,
    regFetch: RegfetchConfig,
    dataMemSize: Int,
    splitAluMem: Boolean = false
)

class CustomWbpf(config: WbpfConfig) extends Component {
  val dataMemory = new DataMemV2(
    c = DataMemConfig(
      numWords = config.dataMemSize
    )
  )
  val pcmgr = new PcManager(c = config.insnBuffer)
  var pcUpdater = new PcUpdater(pcmgr)
  val io = new Bundle {
    val dataMem = slave(dataMemory.use())
    val mmio = slave(Wishbone(MMIOBusConfig()))
    val excOutput = out(CpuException())
  }

  val controller =
    new Controller(insnBufferConfig = config.insnBuffer)
  controller.io.mmio << io.mmio
  controller.io.pcUpdater >> pcUpdater.getUpdater(1)

  val insnBuffer = new InsnBuffer(c = config.insnBuffer)
  controller.io.refill >> insnBuffer.io.refill
  pcmgr.io.stream >> insnBuffer.io.readReq

  val regfile = new Regfetch(c = config.regFetch)

  val regfileReadInput =
    RegGroupContext(c = config.regFetch, dataType = new Bundle)
  val (insnReadToRegfile, insnReadToExec) = StreamFork2(insnBuffer.io.readRsp)
  regfileReadInput.rs1.index := insnReadToRegfile.payload
    .insn(11 downto 8)
    .asUInt // dst
  regfileReadInput.rs2.index := insnReadToRegfile.payload
    .insn(15 downto 12)
    .asUInt // src
  regfile.io.readReq << insnReadToRegfile.translateWith(regfileReadInput)

  val exec = new Exec(
    c = ExecConfig(
      insnFetch = config.insnBuffer,
      regFetch = config.regFetch,
      splitAluMem = config.splitAluMem
    )
  )
  exec.io.regFetch << regfile.io.readRsp
  exec.io.insnFetch << (if (config.regFetch.isAsync) insnReadToExec
                        else insnReadToExec.stage())

  exec.io.regWriteback >> regfile.io.writeReq

  val dm = dataMemory.use()
  exec.io.dataMem.request >> dm.request
  exec.io.dataMem.response << dm.response
  exec.io.branchPcUpdater >> pcUpdater.getUpdater(2)

  io.excOutput := exec.io.excOutput
}

class Wbpf
    extends CustomWbpf(
      WbpfConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 10
        ),
        regFetch = RegfetchConfig(),
        dataMemSize = 1024
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
