package wavebpf

import spinal.core._
import spinal.lib._

object WbpfExt {
  implicit class StreamExt[T <: Data](stream: Stream[T]) {
    def assertProps(checkPayloadInvariance: Boolean = false): Stream[T] = {
      val rValid = RegInit(False)
      val rData = Reg(stream.payloadType)

      val output = Stream(stream.payloadType)
      output << stream

      assert(!(!stream.valid && rValid), "Stream transaction disappeared")
      if (checkPayloadInvariance) {
        assert(
          !rValid || rData === stream.payload,
          "Stream transaction payload changed"
        )
      }

      when(stream.valid && !rValid) {
        rValid := True
        rData := stream.payload
      }

      when(output.fire) {
        rValid := False
      }

      output
    }
  }
}
