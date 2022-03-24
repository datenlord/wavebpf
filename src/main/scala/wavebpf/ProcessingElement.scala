package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

case class PeConfig(
    insnBuffer: InsnBufferConfig,
    regFetch: RegfetchConfig,
    splitAluMem: Boolean = false
)

case class ProcessingElement(config: PeConfig) extends Component {
  val pcmgr = new PcManager(c = config.insnBuffer)
  var pcUpdater = new PcUpdater(pcmgr)
  val io = new Bundle {
    val mmio = slave(Axi4(MMIOBusConfigV2()))
    val excOutput = out(new CpuException())
    val dm = master(DataMemV2Port())
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

  exec.io.dataMem.request >> io.dm.request
  exec.io.dataMem.response << io.dm.response
  exec.io.branchPcUpdater >> pcUpdater.getUpdater(2)

  val excReport = new CpuException()

  when(exec.io.excOutput.code === CpuExceptionCode.PENDING_BRANCH) {
    excReport.assignDontCare()
    excReport.valid := False
  } otherwise {
    excReport := exec.io.excOutput
  }

  io.excOutput := excReport
  controller.io.excReport := excReport
}
