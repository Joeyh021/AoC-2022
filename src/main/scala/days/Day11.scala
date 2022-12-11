package days

import scala.collection.mutable.Buffer

object Day11 extends aoc.Day {

  class MonkeyInfo(
    var items: Buffer[Int],
    val op: Int => Int,
    val test: Int,
    val ifTrue: Int,
    val ifFalse: Int
  ):
    var inspections = 0

  override def solve(input: String): (Any, Any) = {
    val monkeys = input.split("\n\n").map { l =>
      val fields   = l.linesIterator.drop(1).map(_.split(":")(1)).toIndexedSeq
      val starting = (fields(0).split(",")).map(_.trim.toInt).toBuffer
      val test     = (fields(2).split(' ')(3).toInt)
      val t        = (fields(3).split(' ')(4).toInt)
      val f        = (fields(4).split(' ')(4).toInt)
      val op = fields(1).split('=')(1).trim match
        case s"old * old" => (x: Int) => x * x
        case s"old * $i"  => (x: Int) => x * i.toInt
        case s"old + $i"  => (x: Int) => x + i.toInt
      MonkeyInfo(starting, op, test, t, f)
    }

    val nRounds = 20
    for (_ <- 1 to nRounds)
      monkeys.foreach({ monkey =>
        monkey.items.foreach({ item =>
          val worry = monkey.op(item) / 3
          val to    = if worry % monkey.test == 0 then monkey.ifTrue else monkey.ifFalse
          monkeys(to).items.append(worry)
          monkey.inspections += 1
        })
        monkey.items.clear()
      })

    var p1 = monkeys.sortBy(_.inspections).reverse.take(2).map(_.inspections).reduce(_ * _)
    (p1, 0)
  }

}
