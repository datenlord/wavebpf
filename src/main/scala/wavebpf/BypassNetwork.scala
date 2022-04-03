package wavebpf

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

class BypassNetwork[K <: Data, V <: Data](
    keyType: HardType[K],
    valueType: HardType[V]
) extends Area {
  val srcKey = keyType()
  val srcValue = valueType()
  val bypassed = valueType()
  val empty = Bool()
  val hasMatch = Bool()

  val providers = new ArrayBuffer[(Int, Int, Bool, Bool, V)]()

  def provide(
      priority: Int,
      subpriority: Int,
      valid: Bool,
      matches: Bool,
      data: V
  ) = {
    providers += ((priority, subpriority, valid, matches, data))
  }

  Component.current.afterElaboration {
    bypassed := srcValue
    empty := True
    hasMatch := False

    providers
      .sortBy({ x => (x._1, x._2) })
      .reverse
      .foreach((x) => {
        val (prio, subprio, valid, matches, data) = x

        when(valid) {
          /*report(
          Seq(
            "bypass match prio ",
            U(prio, 32 bits),
            " subprio ",
            U(subprio, 32 bits),
            " key ",
            srcKey,
            " value ",
            data
          )
        )*/
          empty := False
          when(matches) {
            hasMatch := True
            bypassed := data
          }
        }
      })
  }
}
