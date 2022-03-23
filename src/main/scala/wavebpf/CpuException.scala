package wavebpf

import spinal.core._
import spinal.lib._

object CpuExceptionCode extends SpinalEnum(binarySequential) {
  val INVALID, NOT_INIT, PENDING_BRANCH, BAD_INSTRUCTION, BAD_MEMORY_ACCESS, EXIT = newElement()
}

case class CpuException() extends Bundle {
  val valid = Bool()
  val code = CpuExceptionCode()
  val data = UInt(64 bits)
}
