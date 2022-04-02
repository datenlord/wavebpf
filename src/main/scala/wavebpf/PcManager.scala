package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class PcUpdateReq() extends Bundle {
  val pc = UInt(29 bits)
  val flush = Bool()
  val flushReason = PcFlushReasonCode()
  val branchSourceValid = Bool()
  val branchSource = UInt(29 bits)
}

object PcFlushReasonCode extends SpinalEnum(binarySequential) {
  val BRANCH_RESOLVE, EXTERNAL, STOP = newElement()
}

class PcManager(c: InsnBufferConfig) extends Component {
  val io = new Bundle {
    val pc = out UInt (29 bits)

    // XXX: This stream can have its payload mutated when valid && !ready.
    val stream = master Stream (InsnBufferReadReq(c))
    val update = slave Flow (PcUpdateReq())
  }

  val currentPc = Reg(UInt(29 bits)) init (0)
  val flush = RegInit(False)
  val flushReason = Reg(PcFlushReasonCode())

  case class BtbEntry() extends Bundle {
    val valid = Bool()
    val source = UInt(c.addrWidth bits)
    val target = UInt(c.addrWidth bits)
    val lru = UInt(log2Up(c.btbSize) bits)
  }

  val btb =
    if (c.useBtb) Vec((0 until c.btbSize).map({ i =>
      val init = BtbEntry()
      init.assignDontCare()
      init.valid := False
      init.lru := i
      val r = Reg(BtbEntry()) init (init)
      r
    }))
    else null

  def lookupBtb(toMatch: UInt): BtbEntry = {
    val matchState = Vec(btb.zipWithIndex.map(x => {
      val (b, i) = x
      val entry = BtbEntry()
      entry.valid := b.valid && b.source === toMatch
      entry.assignUnassignedByName(b)
      entry
    }))
    matchState.reduceBalancedTree((l, r) =>
      l.valid.mux(
        True -> l,
        False -> r
      )
    )
  }

  val btbLookupForCurrentPc =
    if (c.useBtb) lookupBtb(currentPc.resized) else null

  val btbUpdateInit = BtbEntry()
  btbUpdateInit.assignDontCare()
  btbUpdateInit.valid := False

  val btbUpdate = if (c.useBtb) Reg(BtbEntry()) init (btbUpdateInit) else null

  // Write LRU
  if (c.useBtb) {
    when(btbUpdate.valid) {
      val threshold = UInt(log2Up(c.btbSize) bits)
      threshold.assignDontCare()

      val replace = Bool()
      replace := False

      // First, try updating an existing entry
      for ((entry, i) <- btb.zipWithIndex) {
        when(entry.valid && entry.source === btbUpdate.source) {
          // report(Seq("Replacing BTB index " + i))
          entry.target := btbUpdate.target
          entry.lru := c.btbSize - 1
          replace := True
          threshold := entry.lru
        }
      }

      // Then, try popping the oldest entry
      for ((entry, i) <- btb.zipWithIndex) {
        when(entry.lru === 0 && !replace) {
          // report(Seq("Writing to BTB index " + i))
          entry.valid := True
          entry.source := btbUpdate.source
          entry.target := btbUpdate.target
          entry.lru := c.btbSize - 1
          threshold := 0
        }
      }

      // Update LRU counter
      for ((entry, i) <- btb.zipWithIndex) {
        when(entry.lru > threshold) {
          entry.lru := entry.lru - 1
        }
      }
      /*
    report(
      Seq(
        "BTB update -",
        " source ",
        btbUpdate.source,
        " target ",
        btbUpdate.target
      )
    )
    report(
      Seq(
        Seq("LRU state before: "),
        btb.map(x => Seq(x.lru, " ")).flatten
      ).flatten
    )*/
    }
  }

  io.stream.valid := True
  io.stream.payload.addr := currentPc.resized
  io.stream.payload.ctx.flush := flush
  io.stream.payload.ctx.flushReason := flushReason
  if (c.useBtb) {
    io.stream.payload.ctx.prediction.valid := btbLookupForCurrentPc.valid
    io.stream.payload.ctx.prediction.predictedTarget := btbLookupForCurrentPc.target
  } else {
    io.stream.payload.ctx.prediction.valid := False
    io.stream.payload.ctx.prediction.predictedTarget.assignDontCare()
  }

  val rPendingUpdateValid = Reg(Bool()) init (false)
  val rPendingUpdate = Reg(PcUpdateReq())

  when(io.stream.ready) {
    flush := False

    if (c.useBtb) {
      currentPc := btbLookupForCurrentPc.valid.mux(
        True -> btbLookupForCurrentPc.target.resized,
        False -> (currentPc + 1)
      )

      when(btbLookupForCurrentPc.valid) {
        /*report(
        Seq("Prediction: ", currentPc, " -> ", btbLookupForCurrentPc.source, " ", btbLookupForCurrentPc.target)
      )
      report(
        Seq(Seq("LRU state: "), btb.map(x => Seq(x.lru, " ")).flatten).flatten
      )*/
        btbUpdate := btbLookupForCurrentPc
      }
    } else {
      currentPc := currentPc + 1
    }

    when(rPendingUpdateValid) {
      rPendingUpdateValid := False
      currentPc := rPendingUpdate.pc
      flush := rPendingUpdate.flush
      flushReason := rPendingUpdate.flushReason
    }
  }

  when(io.update.valid) {
    when(io.stream.ready) {
      currentPc := io.update.pc
      flush := io.update.flush
      flushReason := io.update.flushReason
    } otherwise {
      rPendingUpdateValid := True
      rPendingUpdate := io.update.payload
    }

    if (c.useBtb) {
      when(io.update.payload.branchSourceValid) {
        btbUpdate.valid := True
        btbUpdate.source := io.update.payload.branchSource.resized
        btbUpdate.target := io.update.pc.resized
      }
    }
  }
}

class PcUpdater(pcmgr: PcManager) extends Area {
  val updaters = ArrayBuffer[(Int, Flow[PcUpdateReq])]()
  val updateStream = Flow(PcUpdateReq())
  pcmgr.io.update << updateStream

  updateStream.setIdle()

  def getUpdater(priority: Int): Flow[PcUpdateReq] = {
    val flow = Flow(PcUpdateReq())
    updaters += ((priority, flow))
    flow
  }

  Component.current.afterElaboration {
    updaters.sortBy(_._1).reverse.foreach { x =>
      when(x._2.valid) {
        updateStream.valid := True
        updateStream.payload := x._2.payload
      }
    }
  }
}
