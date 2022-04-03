package wavebpf

import spinal.core._
import spinal.lib._

object DefaultWbpfConfig {
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
      numPe = 4,
      downsizeDataMemPort = false
    )
}


object NoMemBypassWbpfConfig {
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
        bypassMemOutput = false
      ),
      dataMemSize = 32768,
      numPe = 4,
      downsizeDataMemPort = false
    )
}

object SmallWbpfConfig {
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
      dataMemSize = 16384,
      numPe = 2,
      downsizeDataMemPort = false
    )
}

object NoBtbWbpfConfig {
  def apply() =
    WbpfConfig(
      pe = PeConfig(
        insnBuffer = InsnBufferConfig(
          addrWidth = 11,
          useBtb = false,
          btbSize = 1
        ),
        regFetch = RegfetchConfig(),
        splitAluMem = true,
        bypassMemOutput = true
      ),
      dataMemSize = 32768,
      numPe = 4,
      downsizeDataMemPort = false
    )
}