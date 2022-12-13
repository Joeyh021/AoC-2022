package days
import cats.parse.{ Parser, Parser0 }
import cats.parse.Numbers._
import scala.math.Ordering.Implicits._

object Day13 extends aoc.Day {

  type L[List[_], A] = A match {
    case Int => Int | List[L[List, Int]]
  }
  type Packet = L[List, Int]

  val parsePacket = Parser.recursive[Packet] {
    _.repSep0(Parser.char(',')).with1
      .between(Parser.char('['), Parser.char(']'))
      | digits.map(c => c.toInt)
  }

  given Ordering[Packet] with
    def compare(left: Packet, right: Packet): Int =
      (left, right) match
        case (l: Int, r: Int) if l == r => 0
        case (l: Int, r: Int)           => l.compare(r)
        case (l: Int, rs: List[Packet]) => compare(List(l), rs)
        case (ls: List[Packet], r: Int) => compare(ls, List(r))
        case (l :: ls, r :: rs)         => val c = compare(l, r); if c == 0 then compare(ls, rs) else c
        case (Nil, _ :: _)              => -1
        case (_ :: _, Nil)              => 1
        case (Nil, Nil)                 => 0

  override def solve(input: String): (Any, Any) =
    val p1 =
      input
        .split("\n\n")
        .iterator
        .map(_.linesIterator.map(parsePacket.parseAll(_).toOption.get))
        .map(a => a.next() < a.next())
        .zipWithIndex
        .filter(_._1)
        .map(_._2 + 1)
        .sum

    val markers: Seq[Packet] = Seq(List(List(2)), List(List(6)))
    val packets = input.linesIterator
      .filter(!_.isBlank)
      .toList
      .map(parsePacket.parseAll(_).toOption.get)
      .appendedAll(markers)
      .sorted

    val p2 = (packets.indexOf(markers(0)) + 1) * (packets.indexOf(markers(1)) + 1)

    (p1, p2)
}
