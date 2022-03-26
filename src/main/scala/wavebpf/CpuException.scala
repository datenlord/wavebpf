package wavebpf

import spinal.core._
import spinal.lib._

object CpuExceptionCode extends SpinalEnum {
  val INVALID, NOT_INIT, PENDING_BRANCH, BAD_INSTRUCTION, BAD_MEMORY_ACCESS,
      EXIT, CALL, STOP = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    INVALID -> 0,
    NOT_INIT -> 1,
    PENDING_BRANCH -> 2,
    BAD_INSTRUCTION -> 3,
    BAD_MEMORY_ACCESS -> 4,
    EXIT -> 5,
    CALL -> 6,
    STOP -> 7
  )
}

class CpuExceptionSkeleton() extends Bundle {
  val valid = Bool()
  val code = CpuExceptionCode()
  val data = UInt(64 bits)
}

class CpuException() extends CpuExceptionSkeleton {
  val pc = UInt(29 bits)
}
