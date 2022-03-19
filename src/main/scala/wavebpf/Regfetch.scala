package wavebpf

import spinal.core._
import spinal.lib._

case class RegfetchConfig(
    numRegs: Int = 12,
    isAsync: Boolean = true
);

case class RegContext[T <: Data](
    c: RegfetchConfig,
    dataType: HardType[T]
) extends Bundle {
  val index = UInt(log2Up(c.numRegs) bits)
  val data = dataType()
}

case class RegGroupContext[T <: Data](
    c: RegfetchConfig,
    dataType: HardType[T]
) extends Bundle {
  val rs1 = RegContext(c, dataType)
  val rs2 = RegContext(c, dataType)
}

case class Regfetch(c: RegfetchConfig) extends Component {
  val io = new Bundle {
    val readReq = slave Stream (RegGroupContext(c, new Bundle))
    val readRsp = master Stream (RegGroupContext(c, Bits(64 bits)))
    val writeReq = slave Flow (RegContext(c, Bits(64 bits)))
  }

  val bank0 = Mem(Bits(64 bits), c.numRegs)
  bank0.initialContent = Array.fill(c.numRegs)(0)
  val bank1 = Mem(Bits(64 bits), c.numRegs)
  bank1.initialContent = Array.fill(c.numRegs)(0)

  val writeValid = io.writeReq.valid && io.writeReq.payload.index =/= 0

  bank0.write(
    enable = writeValid,
    address = io.writeReq.payload.index,
    data = io.writeReq.payload.data
  )
  bank1.write(
    enable = writeValid,
    address = io.writeReq.payload.index,
    data = io.writeReq.payload.data
  )

  val readReqStaged = if (c.isAsync) io.readReq else io.readReq.stage()
  val readData = if (c.isAsync) {
    (
      bank0.readAsync(address = io.readReq.rs1.index - 1),
      bank1.readAsync(address = io.readReq.rs2.index - 1)
    )
  } else {
    (
      bank0.readSync(address = io.readReq.rs1.index - 1),
      bank1.readSync(address = io.readReq.rs2.index - 1)
    )
  }

  val rspGroup = RegGroupContext(c, Bits(64 bits))

  rspGroup.rs1.index := readReqStaged.payload.rs1.index
  when(readReqStaged.payload.rs1.index =/= 0) {
    rspGroup.rs1.data := readData._1
  } otherwise {
    rspGroup.rs1.data := 0
  }

  rspGroup.rs2.index := readReqStaged.payload.rs2.index
  when(readReqStaged.payload.rs2.index =/= 0) {
    rspGroup.rs2.data := readData._2
  } otherwise {
    rspGroup.rs2.data := 0
  }

  io.readRsp << readReqStaged.translateWith(rspGroup)
}
