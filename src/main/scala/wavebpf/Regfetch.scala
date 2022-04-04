package wavebpf

import spinal.core._
import spinal.lib._

case class RegfetchConfig(
    numRegs: Int = 16
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
    val readReq = slave Flow (RegGroupContext(c, new Bundle))
    val readRsp = master Flow (RegGroupContext(c, Bits(64 bits)))
    val writeReq = slave Flow (RegContext(c, Bits(64 bits)))

    val replicaReadReq = slave Stream (UInt(log2Up(c.numRegs) bits))
    val replicaReadRsp = master Stream (Bits(64 bits))
  }

  val bank0 = Mem(Bits(64 bits), c.numRegs)
  val bank1 = Mem(Bits(64 bits), c.numRegs)
  val replica = Mem(Bits(64 bits), c.numRegs)

  io.replicaReadRsp << io.replicaReadReq
    .translateWith(
      replica.readAsync(io.replicaReadReq.payload, readUnderWrite = writeFirst)
    )
    .pipelined(m2s = true, s2m = true)

  bank0.write(
    enable = io.writeReq.valid,
    address = io.writeReq.payload.index,
    data = io.writeReq.payload.data
  )
  bank1.write(
    enable = io.writeReq.valid,
    address = io.writeReq.payload.index,
    data = io.writeReq.payload.data
  )
  replica.write(
    enable = io.writeReq.valid,
    address = io.writeReq.payload.index,
    data = io.writeReq.payload.data
  )

  /*when(io.writeReq.valid) {
    report(
      Seq(
        "Reg write ",
        io.writeReq.payload.index,
        " ",
        io.writeReq.payload.data
      )
    )
  }*/

  val readData = (
    bank0.readAsync(
      address = io.readReq.rs1.index,
      readUnderWrite = writeFirst
    ),
    bank1.readAsync(
      address = io.readReq.rs2.index,
      readUnderWrite = writeFirst
    )
  )

  val rspGroup = RegGroupContext(c, Bits(64 bits))

  rspGroup.rs1.index := io.readReq.payload.rs1.index
  rspGroup.rs1.data := readData._1

  rspGroup.rs2.index := io.readReq.payload.rs2.index
  rspGroup.rs2.data := readData._2

  io.readRsp << io.readReq.translateWith(rspGroup)
}
