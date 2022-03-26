package wavebpf

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

object WbpfUtil {
  def decodeAxSize(axSize: UInt): UInt = {
    val ret = UInt(32 bits)
    switch(axSize) {
      is(0x0) { ret := 1 }
      is(0x1) { ret := 2 }
      is(0x2) { ret := 4 }
      is(0x3) { ret := 8 }
      is(0x4) { ret := 16 }
      is(0x5) { ret := 32 }
      is(0x6) { ret := 64 }
      is(0x7) { ret := 128 }
    }
    ret
  }

  def axi4Pipe(x: Axi4): Axi4 = {
    val sink = Axi4(x.config)

    sink.ar << x.ar.s2mPipe()
    sink.aw << x.aw.s2mPipe()
    sink.w << x.w.s2mPipe()
    sink.r.m2sPipe() >> x.r
    sink.b.m2sPipe() >> x.b

    sink
  }

  def axilite4Pipe(x: AxiLite4): AxiLite4 = {
    val sink = AxiLite4(x.config)

    sink.ar << x.ar.s2mPipe()
    sink.aw << x.aw.s2mPipe()
    sink.w << x.w.s2mPipe()
    sink.r.m2sPipe() >> x.r
    sink.b.m2sPipe() >> x.b

    sink
  }

  def truncateAxiLite4Address(
      x: AxiLite4Ax,
      range: Range.Inclusive
  ): AxiLite4Ax = {
    val ret = AxiLite4Ax(x.config)
    ret.addr := x.addr(range).resized
    ret.assignUnassignedByName(x)
    ret
  }

  def axi4Pipe(x: Axi4WriteOnly): Axi4WriteOnly = {
    val sink = Axi4WriteOnly(x.config)

    sink.aw << x.aw.s2mPipe()
    sink.w << x.w.s2mPipe()
    sink.b.m2sPipe() >> x.b

    sink
  }

  def axi4Pipe(x: Axi4ReadOnly): Axi4ReadOnly = {
    val sink = Axi4ReadOnly(x.config)

    sink.ar << x.ar.s2mPipe()
    sink.r.m2sPipe() >> x.r

    sink
  }
}
