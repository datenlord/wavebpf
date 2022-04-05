package wavebpf

import spinal.core._
import spinal.lib._

object DefaultWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11,
          useBtb = false,
          btbSize = 0
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = true
      ),
      dataMemSize = 32768,
      numPe = 4
    )
}


object NoMemBypassWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11,
          useBtb = false,
          btbSize = 0
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = false
      ),
      dataMemSize = 32768,
      numPe = 4
    )
}

object SmallWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11,
          useBtb = false,
          btbSize = 0
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = true
      ),
      dataMemSize = 16384,
      numPe = 2,
      dataMemNumBanks = 2
    )
}

object WithBtbWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11,
          useBtb = true,
          btbSize = 12
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = true
      ),
      dataMemSize = 32768,
      numPe = 4
    )
}