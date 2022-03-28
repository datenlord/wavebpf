package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class PcUpdateReq() extends Bundle {
  val pc = UInt(29 bits)
  val flush = Bool()
  val flushReason = PcFlushReasonCode()
}

object PcFlushReasonCode extends SpinalEnum(binarySequential) {
  val BRANCH_RESOLVE, EXTERNAL, STOP = newElement()
}

class PcManager(c: InsnBufferConfig) extends Component {
  val io = new Bundle {
    val pc = out UInt (29 bits)

    // XXX: This stream can have its payload mutated when valid && !ready.
    val stream = master Stream (InsnBufferReadReq(c))
    val update = slave Flow (PcUpdateReq())
  }

  val currentPc = Reg(UInt(29 bits)) init (0)
  val flush = RegInit(False)
  val flushReason = Reg(PcFlushReasonCode())

  io.stream.valid := True
  io.stream.payload.addr := currentPc.resized
  io.stream.payload.ctx.flush := flush
  io.stream.payload.ctx.flushReason := flushReason

  when(io.stream.ready) {
    currentPc := currentPc + 1
    flush := False
  }

  when(io.update.valid) {
    currentPc := io.update.pc
    flush := io.update.flush
    flushReason := io.update.flushReason
  }
}

class PcUpdater(pcmgr: PcManager) extends Area {
  val updaters = ArrayBuffer[(Int, Flow[PcUpdateReq])]()
  val updateStream = Flow(PcUpdateReq())
  pcmgr.io.update << updateStream

  updateStream.setIdle()

  def getUpdater(priority: Int): Flow[PcUpdateReq] = {
    val flow = Flow(PcUpdateReq())
    updaters += ((priority, flow))
    flow
  }

  Component.current.afterElaboration {
    updaters.sortBy(_._1).reverse.foreach { x =>
      when(x._2.valid) {
        updateStream.valid := True
        updateStream.payload := x._2.payload
      }
    }
  }
}
