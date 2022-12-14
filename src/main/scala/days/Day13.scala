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
        case (l: Int, rs)               => compare(List(l), rs)
        case (ls, r: Int)               => compare(ls, List(r))
        case (l :: ls, r :: rs)         => val c = compare(l, r); if c == 0 then compare(ls, rs) else c
        case (Nil, Nil)                 => 0
        case (Nil, _)                   => -1
        case (_, Nil)                   => 1

  override def solve(input: String): (Any, Any) =
    val p1 =
      input
        .split("\n\n")
        .map(_.linesIterator.map(parsePacket.parseAll(_).toOption.get))
        .map(a => a.next() < a.next())
        .zipWithIndex
        .filter(_._1)
        .map(_._2 + 1)
        .sum

    val markers = IndexedSeq[Packet](List(List(2)), List(List(6)))
    val packets = input.linesIterator
      .filter(!_.isBlank)
      .map(parsePacket.parseAll(_).toOption.get)
      .toList
      .appendedAll(markers)
      .sorted

    val p2 = (packets.indexOf(markers(0)) + 1) * (packets.indexOf(markers(1)) + 1)

    (p1, p2)
}
