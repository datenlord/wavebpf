package wavebpf

import spinal.core._
import spinal.lib._

object ControlFlowPriority {
  private val width = 3 bits
  def forFlushReason(that: SpinalEnumCraft[PcFlushReasonCode.type]): UInt = {
    val ret = UInt(width)
    when(that === PcFlushReasonCode.EXTERNAL || that === PcFlushReasonCode.STOP) {
      ret := 0
    } elsewhen (that === PcFlushReasonCode.BRANCH_RESOLVE) {
      ret := 1
    } otherwise {
      ret := 2
    }
    ret
  }

  def forException(that: SpinalEnumCraft[CpuExceptionCode.type]): UInt = {
    val ret = UInt(width)
    when(that === CpuExceptionCode.PENDING_BRANCH) {
      ret := 1
    } otherwise {
      ret := 0
    }
    ret
  }
}
