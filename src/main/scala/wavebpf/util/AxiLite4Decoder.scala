package wavebpf.util

import spinal.core._
import spinal.lib._

import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping

case class AxiLite4ReadOnlyDecoder(axiConfig: AxiLite4Config,decodings : Seq[SizeMapping],pendingMax : Int = 7) extends Component{

  assert(!SizeMapping.verifyOverlapping(decodings), "AXI4 address decoding overlapping")

  val io = new Bundle{
    val input = slave(AxiLite4ReadOnly(axiConfig))
    val outputs = Vec(master(AxiLite4ReadOnly(axiConfig)),decodings.size)
  }

  val pendingCmdCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.readCmd.fire,
    decWhen = io.input.readRsp.fire
  )
  val decodedCmdSels = decodings.map(_.hit(io.input.readCmd.addr) && io.input.readCmd.valid).asBits
  val decodedCmdError = decodedCmdSels === 0
  val pendingSels  = RegNextWhen(decodedCmdSels,io.input.readCmd.ready)  init(0)
  val pendingError = RegNextWhen(decodedCmdError,io.input.readCmd.ready)  init(False)
  val allowCmd    = pendingCmdCounter === 0 || (pendingCmdCounter =/= pendingMax && pendingSels === decodedCmdSels)

  //Decoding error managment
  val decodingErrorPossible = decodings.map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val errorSlave = if(decodingErrorPossible) AxiLite4ReadOnlyErrorSlave(axiConfig) else null

  //Wire readCmd
  io.input.readCmd.ready := ((decodedCmdSels & io.outputs.map(_.readCmd.ready).asBits).orR || (if(decodingErrorPossible) (decodedCmdError && errorSlave.io.axi.readCmd.ready) else False))  && allowCmd
  if(decodingErrorPossible) {
    errorSlave.io.axi.readCmd.valid := io.input.readCmd.valid && decodedCmdError && allowCmd
    errorSlave.io.axi.readCmd.payload := io.input.readCmd.payload
  }
  for((output,sel) <- (io.outputs,decodedCmdSels.asBools).zipped){
    output.readCmd.valid := io.input.readCmd.valid && sel && allowCmd
    output.readCmd.payload := io.input.readCmd.payload
  }

  //Wire ReadRsp
  val readRspIndex = OHToUInt(pendingSels)

  io.input.readRsp.valid := io.outputs.map(_.readRsp.valid).asBits.orR
  io.input.readRsp.payload := MuxOH(pendingSels,io.outputs.map(_.readRsp.payload))
  if(decodingErrorPossible) {
    io.input.readRsp.valid setWhen(errorSlave.io.axi.readRsp.valid)
    when(pendingError){
    io.input.readRsp.resp := errorSlave.io.axi.readRsp.resp
    }
    errorSlave.io.axi.readRsp.ready := io.input.readRsp.ready
  }
  io.outputs.foreach(_.readRsp.ready := io.input.readRsp.ready)
}


case class AxiLite4WriteOnlyDecoder(axiConfig: AxiLite4Config,decodings : Seq[SizeMapping],pendingMax : Int = 7) extends Component{
  assert(!SizeMapping.verifyOverlapping(decodings), "AXI4 address decoding overlapping")

  val io = new Bundle{
    val input = slave(AxiLite4WriteOnly(axiConfig))
    val outputs = Vec(master(AxiLite4WriteOnly(axiConfig)),decodings.size)
  }

  val cmdAllowedStart = Bool()

  val pendingCmdCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.writeCmd.fire,
    decWhen = io.input.writeRsp.fire
  )

  val pendingDataCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = cmdAllowedStart,
    decWhen = io.input.writeData.fire
  )

  val decodedCmdSels = decodings.map(_.hit(io.input.writeCmd.addr) && io.input.writeCmd.valid).asBits
  val decodedCmdError = decodedCmdSels === 0
  val pendingSels  = RegNextWhen(decodedCmdSels,cmdAllowedStart)  init(0)
  val pendingError = RegNextWhen(decodedCmdError,cmdAllowedStart)  init(False)
  val allowCmd    = pendingCmdCounter === 0 || (pendingCmdCounter =/= pendingMax && pendingSels === decodedCmdSels)
  val allowData   = pendingDataCounter =/= 0
  cmdAllowedStart := io.input.writeCmd.valid && allowCmd && (RegInit(True) clearWhen(cmdAllowedStart) setWhen(io.input.writeCmd.ready))

  //Decoding error managment
  val decodingErrorPossible = decodings.map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val errorSlave = if(decodingErrorPossible) AxiLite4WriteOnlyErrorSlave(axiConfig) else null

  //Wire writeCmd
  io.input.writeCmd.ready := ((decodedCmdSels & io.outputs.map(_.writeCmd.ready).asBits).orR || (if(decodingErrorPossible) (decodedCmdError && errorSlave.io.axi.writeCmd.ready) else False)) && allowCmd
  if(decodingErrorPossible) {
    errorSlave.io.axi.writeCmd.valid := io.input.writeCmd.valid && decodedCmdError && allowCmd
    errorSlave.io.axi.writeCmd.payload := io.input.writeCmd.payload
  }
  for((output,sel) <- (io.outputs,decodedCmdSels.asBools).zipped){
    output.writeCmd.valid := io.input.writeCmd.valid && sel && allowCmd
    output.writeCmd.payload := io.input.writeCmd.payload
  }

  //Wire writeData
  io.input.writeData.ready := ((pendingSels & io.outputs.map(_.writeData.ready).asBits).orR || (if(decodingErrorPossible) (pendingError && errorSlave.io.axi.writeData.ready) else False)) && allowData
  if(decodingErrorPossible) {
    errorSlave.io.axi.writeData.valid := io.input.writeData.valid && pendingError && allowData
    errorSlave.io.axi.writeData.payload := io.input.writeData.payload
  }
  for((output,sel) <- (io.outputs,pendingSels.asBools).zipped){
    output.writeData.valid   := io.input.writeData.valid && sel && allowData
    output.writeData.payload := io.input.writeData.payload
  }

  //Wire writeRsp
  val writeRspIndex = OHToUInt(pendingSels)
  io.input.writeRsp.valid := io.outputs.map(_.writeRsp.valid).asBits.orR || (if(decodingErrorPossible) errorSlave.io.axi.writeRsp.valid else False)
  io.input.writeRsp.payload := MuxOH(pendingSels,io.outputs.map(_.writeRsp.payload))
  if(decodingErrorPossible) {
    when(pendingError){
      io.input.writeRsp.resp := errorSlave.io.axi.writeRsp.resp
    }
    errorSlave.io.axi.writeRsp.ready := io.input.writeRsp.ready
  }
  io.outputs.foreach(_.writeRsp.ready := io.input.writeRsp.ready)
}
