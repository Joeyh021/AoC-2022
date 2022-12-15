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
    else (start._1 to end._1 by (if start._1 > end._1 then -1 else 1)).zip(Iterator.continually(start._2))

  def addNewSand(withFloor: Boolean)(floor: Int)(cave: Map[Coord, Square]): Option[Coord] = {
    @tailrec
    def p1(pos: Coord): Option[Coord] =
      // if oob, stop trying to place
      if pos._2 >= floor then return None

      val sq = cave.getOrElse(pos, Empty)
      // if we're in empty space, this move was good and keep going
      if sq == Empty then p1(pos._1, pos._2 + 1)
      else {
        // try go left
        if cave.getOrElse((pos._1 - 1, pos._2), Empty) == Empty then p1(pos._1 - 1, pos._2)
        // try go right
        else if cave.getOrElse((pos._1 + 1, pos._2), Empty) == Empty then p1(pos._1 + 1, pos._2)
        // settle
        else Some((pos._1, pos._2 - 1))
      }

    extension (cave: Map[Coord, Square]) {
      def getOrFloor(k: Coord) = if k._2 >= floor then Rock else cave.getOrElse(k, Empty)
    }

    @tailrec
    def p2(pos: Coord): Option[Coord] =

      val sq = cave.getOrFloor(pos)
      // if blocked entrance
      if sq == Sand && pos == (500, 0) then return None
      // if we're in empty space, this move was good and keep going
      else if sq == Empty then p2(pos._1, pos._2 + 1)
      else {
        // try go left
        if cave.getOrFloor((pos._1 - 1, pos._2)) == Empty then p2(pos._1 - 1, pos._2)
        // try go right
        else if cave.getOrFloor((pos._1 + 1, pos._2)) == Empty then p2(pos._1 + 1, pos._2)
        // settle
        else Some((pos._1, pos._2 - 1))
      }

    if withFloor then p2(500, 0) else p1(500, 0)
  }

  override def solve(input: String) =
    val cave = input.linesIterator
      .map(_.split("->").map { x =>
        val p = x.split(",").map(_.trim.toInt); (p(0), p(1))
      })
      .foldLeft(Map[Coord, Square]()) { (cave, coords) =>
        cave ++ coords.iterator.sliding(2).flatMap(p => coordRange(p(0), p(1)).zip(Iterator continually Rock))
      }

    val abyss = cave.keys.map(_._2).max + 1
    val floor = cave.keys.map(_._2).max + 2

    @tailrec
    def run[S](s: S, f: (S => Option[S])): S =
      f(s) match
        case None     => s
        case Some(ns) => run(ns, f)

    val rp1 = addNewSand(false)(abyss)
    val rp2 = addNewSand(true)(floor)

    val countSand = (f: Map[Coord, Square] => Option[Coord]) =>
      run(cave, (cave => f(cave).map(ns => cave + (ns -> Sand)))).values.count(_ == Sand)

    (countSand(rp1), countSand(rp2))

}
