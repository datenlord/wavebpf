package wavebpf.util

import spinal.core._
import spinal.lib.slave
import spinal.lib.bus.amba4.axilite._

/**
 * Created by spinalvm on 13.06.17.
 */
case class AxiLite4WriteOnlyErrorSlave(axiConfig: AxiLite4Config) extends Component{
  val io = new Bundle{
    val axi = slave(AxiLite4WriteOnly(axiConfig))
  }
  val consumeData = RegInit(False)
  val sendRsp     = RegInit(False)

  io.axi.writeCmd.ready := !(consumeData || sendRsp)
  when(io.axi.writeCmd.fire){
    consumeData := True
  }

  io.axi.writeData.ready := consumeData
  when(io.axi.writeData.fire){
    consumeData := False
    sendRsp := True
  }

  io.axi.writeRsp.valid := sendRsp
  io.axi.writeRsp.setDECERR
  when(io.axi.writeRsp.fire){
    sendRsp := False
  }
}


case class AxiLite4ReadOnlyErrorSlave(axiConfig: AxiLite4Config) extends Component{
  val io = new Bundle{
    val axi = slave(AxiLite4ReadOnly(axiConfig))
  }

  val sendRsp       = RegInit(False)
  val remaining     = Reg(UInt(8 bits))
  val remainingZero = remaining === 0

  io.axi.readCmd.ready := !sendRsp
  when(io.axi.readCmd.fire){
    sendRsp := True
    remaining := U(0)
  }

  io.axi.readRsp.valid := sendRsp
  io.axi.readRsp.setDECERR

  when(sendRsp) {
    when(io.axi.readRsp.ready) {
      remaining := remaining - 1
      when(remainingZero) {
        sendRsp := False
      }
    }
  }
}
