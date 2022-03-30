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

  def lookupBtb(toMatch: UInt): BtbEntry = btb.reduceBalancedTree((l, r) =>
    (l.valid && l.source === toMatch).mux(
      True -> l,
      False -> r
    )
  )

  val btb = Vec((0 until c.btbSize).map({ i =>
    val init = BtbEntry()
    init.assignDontCare()
    init.valid := False
    init.lru := i
    val r = Reg(BtbEntry()) init (init)
    r
  }))

  val btbLookupForCurrentPc = lookupBtb(currentPc.resized)

  val btbUpdate = BtbEntry()
  btbUpdate.assignDontCare()
  btbUpdate.valid := False

  // Write LRU
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

  io.stream.valid := True
  io.stream.payload.addr := currentPc.resized
  io.stream.payload.ctx.flush := flush
  io.stream.payload.ctx.flushReason := flushReason
  io.stream.payload.ctx.prediction.valid := btbLookupForCurrentPc.valid
  io.stream.payload.ctx.prediction.predictedTarget := btbLookupForCurrentPc.target

  when(io.stream.ready) {
    currentPc := btbLookupForCurrentPc.valid.mux(
      True -> btbLookupForCurrentPc.target.resized,
      False -> (currentPc + 1)
    )
    flush := False

    when(btbLookupForCurrentPc.valid) {
      /*report(
        Seq("Prediction: ", currentPc, " -> ", btbLookupForCurrentPc.target)
      )
      report(
        Seq(Seq("LRU state: "), btb.map(x => Seq(x.lru, " ")).flatten).flatten
      )*/
      btbUpdate := btbLookupForCurrentPc
    }
  }

  when(io.update.valid) {
    currentPc := io.update.pc
    flush := io.update.flush
    flushReason := io.update.flushReason

    when(io.update.payload.branchSourceValid) {
      btbUpdate.valid := True
      btbUpdate.source := io.update.payload.branchSource.resized
      btbUpdate.target := io.update.pc.resized
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
