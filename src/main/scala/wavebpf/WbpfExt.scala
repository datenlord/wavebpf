package wavebpf

import spinal.core._
import spinal.lib._

object WbpfExt {
  implicit class StreamExt[T <: Data](stream: Stream[T]) {
    def check(
        payloadInvariance: Boolean = false,
        payloadChangeFormatter: (T, T) => Seq[Any] = null
    ): Stream[T] = {
      val rValid = RegInit(False) setWhen (stream.valid) clearWhen (stream.fire)
      val rData = RegNextWhen(stream.payload, stream.valid && !rValid)

      val stack = ScalaLocated.long.replace("\n", "\\n")
      assert(
        !(!stream.valid && rValid),
        "Stream transaction disappeared:\\n" + stack
      )
      if (payloadInvariance) {
        val baseMsg = "Stream transaction payload changed:\\n" + stack
        val msg: Seq[Any] =
          if (payloadChangeFormatter == null) Seq(baseMsg)
          else Seq(baseMsg) ++ payloadChangeFormatter(rData, stream.payload)
        assert(
          !rValid || rData === stream.payload,
          msg
        )
      }

      stream
    }
  }
}
