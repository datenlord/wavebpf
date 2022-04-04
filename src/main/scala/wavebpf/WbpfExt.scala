package wavebpf

import spinal.core._
import spinal.lib._

object WbpfExt {
  implicit class StreamExt[T <: Data](stream: Stream[T]) {
    def check(payloadInvariance: Boolean = false): Stream[T] = {
      val rValid = RegInit(False) setWhen (stream.valid) clearWhen (stream.fire)
      val rData = RegNextWhen(stream.payload, stream.valid && !rValid)

      val stack = ScalaLocated.long.replace("\n", "\\n")
      assert(
        !(!stream.valid && rValid),
        "Stream transaction disappeared:\\n" + stack
      )
      if (payloadInvariance) {
        assert(
          !rValid || rData === stream.payload,
          "Stream transaction payload changed:\\n" + stack
        )
      }

      stream
    }
  }
}
