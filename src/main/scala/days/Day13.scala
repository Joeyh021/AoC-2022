package days
import cats.parse.{ Parser, Parser0 }
import cats.parse.Numbers._
import scala.math.Ordering.Implicits._

object Day13 extends aoc.Day {

  type L[List[_], A] = A match {
    case Int => Int | List[L[List, Int]]
  }
  type Packet = L[List, Int]

  // enum Packet:
  //   case I(i: Int)
  //   case L(ps: List[Packet])

  // import Packet._

  // given Conversion[Int, Packet.I] with
  //   def apply(i: Int) = Packet.I(i)

  // given Conversion[List[Packet], Packet.L] with
  //   def apply(l: List[Packet]) = Packet.L(l)

  val parsePacket: Parser[Packet] = Parser.recursive { r =>
    val list: Parser[Packet] = r
      .repSep0(Parser.char(','))
      .with1
      .between(Parser.char('['), Parser.char(']'))
    val num: Parser[Packet] = digits.map(c => c.toInt)

    list | num
  }

  def isOrdered(left: Packet, right: Packet): Option[Boolean] =
    (left, right) match
      case (l: Int, r: Int) if l == r => None
      case (l: Int, r: Int)           => Some(l < r)
      case (l: Int, rs: List[Packet]) => isOrdered(List(l), rs)
      case (ls: List[Packet], r: Int) => isOrdered(ls, List(r))
      case (l :: ls, r :: rs)         => isOrdered(l, r) orElse isOrdered(ls, rs)
      case (Nil, _ :: _)              => Some(true)
      case (_ :: _, Nil)              => Some(false)
      case (Nil, Nil)                 => None

  override def solve(input: String): (Any, Any) =
    val p1 =
      input
        .split("\n\n")
        .iterator
        .map(_.linesIterator.map(parsePacket.parseAll(_).getOrElse(-1)).toList)
        .map(a => isOrdered(a.head, a.last).get)
        .zipWithIndex
        .toList
        .filter(_._1)
        .map(_._2 + 1)
        .sum
    (p1, 0)
}
