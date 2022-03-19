package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class PcUpdateReq() extends Bundle {
  val pc = UInt(61 bits)
  val flush = Bool()
}

class PcManager(c: InsnBufferConfig) extends Component {
  val io = new Bundle {
    val pc = out UInt (61 bits)
    val stream = master Stream (InsnBufferReadReq(c))
    val update = slave Flow (PcUpdateReq())
  }

  val currentPc = Reg(UInt(61 bits)) init (0)
  val flush = RegInit(False)

  io.stream.valid := True
  io.stream.payload.addr := currentPc.resized
  io.stream.payload.ctx.flush := flush

  when(io.stream.ready) {
    currentPc := currentPc + 1
    flush := False
  }

  when(io.update.valid) {
    currentPc := io.update.pc
    flush := io.update.flush
  }
}

class PcUpdater(pcmgr: PcManager) extends Area {
  val updaters = ArrayBuffer[Flow[PcUpdateReq]]()
  val updateStream = Flow (PcUpdateReq())
  pcmgr.io.update << updateStream

  updateStream.setIdle()

  def getUpdater: Flow[PcUpdateReq] = {
    val flow = Flow(PcUpdateReq())
    updaters += flow
    flow
  }

  Component.current.afterElaboration {
    updaters.foreach { x =>
      when(x.valid) {
        updateStream.valid := True
        updateStream.payload := x.payload
      }
    }
  }
}
