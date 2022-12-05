package days

import scala.collection.mutable.ArrayBuffer

object Day5 extends aoc.Day:
  type Stacks = ArrayBuffer[ArrayBuffer[Char]]

  // the top bit of the input, manually parsed
  val init =
    ArrayBuffer(
      ArrayBuffer('H', 'C', 'R'),
      ArrayBuffer('B', 'J', 'H', 'L', 'S', 'F'),
      ArrayBuffer('R', 'M', 'D', 'H', 'J', 'T', 'Q'),
      ArrayBuffer('S', 'G', 'R', 'H', 'Z', 'B', 'J'),
      ArrayBuffer('R', 'P', 'F', 'Z', 'T', 'D', 'C', 'B'),
      ArrayBuffer('T', 'H', 'C', 'G'),
      ArrayBuffer('S', 'N', 'V', 'Z', 'B', 'P', 'W', 'L'),
      ArrayBuffer('R', 'J', 'Q', 'G', 'C'),
      ArrayBuffer('L', 'D', 'T', 'R', 'H', 'P', 'F', 'S')
    )

  def p1(stacks: Stacks, x: (Int, Int, Int)): Stacks = {
    val crates = stacks(x._2).takeRight(x._1)
    stacks(x._2).dropRightInPlace(x._1)
    stacks(x._3) ++= crates.reverse
    stacks
  }

  def p2(stacks: Stacks, x: (Int, Int, Int)): Stacks = {
    val crates = stacks(x._2).takeRight(x._1)
    stacks(x._2).dropRightInPlace(x._1)
    stacks(x._3) ++= crates
    stacks
  }

  override def solve(input: Seq[String]): (Any, Any) =
    val pattern = """move (\d*) from (\d*) to (\d*)""".r
    val parsed = input
      .map(
        _ match
          case pattern(amnt, from, to) => (amnt.toInt, from.toInt - 1, to.toInt - 1)
      )

    def run(f: (Stacks, (Int, Int, Int)) => Stacks): String = parsed
      .foldLeft(init)(f)
      .map(_.last)
      .mkString

    (
      parsed
        .foldLeft(init.map(_.clone))(p1)
        .map(_.last)
        .mkString,
      parsed
        .foldLeft(init.map(_.clone))(p2)
        .map(_.last)
        .mkString
    )
