package days

import scala.annotation.tailrec
import util.control.Breaks._

object Day14 extends aoc.Day {
  enum Square:
    case Empty
    case Sand
    case Rock
  import Square._

  type Coord = (Int, Int)

  def coordRange(start: Coord, end: Coord): Seq[Coord] =
    if start == end then Seq(start)
    else if start._1 == end._1 then
      (Iterator.continually(start._1)).zip(start._2 to end._2 by (if start._2 > end._2 then -1 else 1)).toSeq
    else if start._2 == end._2 then
      (start._1 to end._1 by (if start._1 > end._1 then -1 else 1)).zip(Iterator.continually(start._2))
    else throw Exception()

  def addNewSand(min: Int)(cave: Map[Coord, Square]): Option[Coord] = {

    @tailrec
    def r(pos: Coord): Option[Coord] =
      // if oob, stop trying to place
      if pos._2 >= min then return None

      val sq = cave.getOrElse(pos, Empty)
      // if we're in empty space, this move was good and keep going
      if sq == Empty then r(pos._1, pos._2 + 1)
      else {
        // try go left
        if cave.getOrElse((pos._1 - 1, pos._2), Empty) == Empty then r(pos._1 - 1, pos._2)
        // try go right
        else if cave.getOrElse((pos._1 + 1, pos._2), Empty) == Empty then r(pos._1 + 1, pos._2)
        // settle
        else Some((pos._1, pos._2 - 1))
      }

    r(500, 0)
  }

  override def solve(input: String) =
    val parsed = input.linesIterator
      .map(_.split("->").map { x =>
        val p = x.split(",").map(_.trim.toInt); (p(0), p(1))
      }.toIndexedSeq)
      .toIndexedSeq

    var cave = parsed.foldLeft(Map[Coord, Square]()) { (cave, coords) =>
      cave ++ coords.iterator.sliding(2).flatMap(p => coordRange(p(0), p(1)).zip(Iterator continually Rock))
    }

    val abyss  = parsed.flatten.map(_._2).max + 1
    val doSand = addNewSand(abyss)

    @tailrec
    def run[S](s: S, f: (S => Option[S])): S =
      f(s) match
        case None     => s
        case Some(ns) => run(ns, f)

    (run(cave, (cave => doSand(cave).map(ns => cave + (ns -> Sand)))).values.count(_ == Sand), 0)

}
