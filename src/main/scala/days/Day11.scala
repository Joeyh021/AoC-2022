package days

import scala.collection.mutable.Buffer

object Day11 extends aoc.Day {
  class MonkeyInfo(
    var items: Buffer[Long],
    val op: Long => Long,
    val test: Int,
    val ifTrue: Int,
    val ifFalse: Int
  ):
    var inspections: Long = 0

    override def clone(): MonkeyInfo =
      MonkeyInfo(
        this.items.clone(),
        this.op,
        this.test,
        this.ifTrue,
        this.ifFalse
      )

  def playWithMonkeys(monkeys: Seq[MonkeyInfo], n: Int, meds: Long => Long) =
    for (_ <- 1 to n)
      monkeys
        .foreach({ monkey =>
          monkey.items.foreach({ item =>
            val worry = meds(monkey.op(item))
            val to    = if worry % monkey.test == 0 then monkey.ifTrue else monkey.ifFalse
            monkeys(to).items.append(worry)
            monkey.inspections += 1
          })
          monkey.items.clear()
        })
    monkeys
      .sortBy(_.inspections)
      .reverse
      .take(2)
      .map(_.inspections)
      .reduce(_ * _)

  override def solve(input: String): (Any, Any) = {
    val monkeys = input.split("\n\n").map { l =>
      val fields   = l.linesIterator.drop(1).map(_.split(":")(1)).toIndexedSeq
      val starting = (fields(0).split(",")).map(_.trim.toLong).toBuffer
      val test     = (fields(2).split(' ')(3).toInt)
      val t        = (fields(3).split(' ')(4).toInt)
      val f        = (fields(4).split(' ')(4).toInt)
      val op = fields(1).split('=')(1).trim match
        case s"old * old" => (x: Long) => x * x
        case s"old * $i"  => (x: Long) => x * i.toLong
        case s"old + $i"  => (x: Long) => x + i.toLong
      MonkeyInfo(starting, op, test, t, f)
    }

    val p1  = playWithMonkeys(monkeys.toSeq.map(_.clone), 20, (x: Long) => x / 3)
    val lcm = monkeys.toSeq.map(_.test).reduce(_ * _)
    val p2  = playWithMonkeys(monkeys.toSeq.map(_.clone), 10000, (x: Long) => x % lcm)

    (p1, p2)

  }

}
