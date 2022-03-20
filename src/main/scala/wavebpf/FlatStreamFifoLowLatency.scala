package wavebpf

import spinal.core._
import spinal.lib._

// Modified from SpinalHDL `StreamFifoLowLatency`.
class FlatStreamFifoLowLatency[T <: Data](
    val dataType: HardType[T],
    val depth: Int,
    val latency: Int = 0,
    useVec: Boolean = false
) extends Component {
  require(depth >= 1)
  val io = new Bundle with StreamFifoInterface[T] {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool () default (False)
    val occupancy = out UInt (log2Up(depth + 1) bit)
    override def pushOccupancy: UInt = occupancy
    override def popOccupancy: UInt = occupancy
    val currentVec = out Vec (dataType, depth)
    val currentPushPtr = out UInt (log2Up(depth) bit)
    val currentPopPtr = out UInt (log2Up(depth) bit)
  }
  val vec = useVec generate Vec(Reg(dataType), depth)
  val ram = !useVec generate Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  val pushing = io.push.fire
  val popping = io.pop.fire

  io.push.ready := !full

  io.currentPushPtr := pushPtr.value
  io.currentPopPtr := popPtr.value
  if (useVec) {
    io.currentVec := vec
  } else {
    io.currentVec.assignDontCare()
  }

  val readed = if (useVec) vec(popPtr.value) else ram(popPtr.value)

  latency match {
    case 0 => {
      when(!empty) {
        io.pop.valid := True
        io.pop.payload := readed
      } otherwise {
        io.pop.valid := io.push.valid
        io.pop.payload := io.push.payload
      }
    }
    case 1 => {
      io.pop.valid := !empty
      io.pop.payload := readed
    }
  }
  when(pushing =/= popping) {
    risingOccupancy := pushing
  }
  when(pushing) {
    if (useVec)
      vec.write(pushPtr.value, io.push.payload)
    else
      ram.write(pushPtr.value, io.push.payload)
    pushPtr.increment()
  }
  when(popping) {
    popPtr.increment()
  }

  val ptrDif = pushPtr - popPtr
  if (isPow2(depth))
    io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).asUInt
  else {
    when(ptrMatch) {
      io.occupancy := Mux(risingOccupancy, U(depth), U(0))
    } otherwise {
      io.occupancy := Mux(pushPtr > popPtr, ptrDif, U(depth) + ptrDif)
    }
  }

  when(io.flush) {
    pushPtr.clear()
    popPtr.clear()
    risingOccupancy := False
  }
}
