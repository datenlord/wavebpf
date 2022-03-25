package wavebpf

import spinal.core._
import spinal.lib._

object WbpfUtil {
  def decodeAxSize(axSize: UInt): UInt = {
    val ret = UInt(32 bits)
    switch(axSize) {
      is(0x0) { ret := 1 }
      is(0x1) { ret := 2 }
      is(0x2) { ret := 4 }
      is(0x3) { ret := 8 }
      is(0x4) { ret := 16 }
      is(0x5) { ret := 32 }
      is(0x6) { ret := 64 }
      is(0x7) { ret := 128 }
    }
    ret
  }
}
