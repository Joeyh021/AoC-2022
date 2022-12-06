package days

import aoc.Day
import scala.util.Using
import scala.io.Source

//Day 1 from 2021
object Day0 extends Day {
  override def solve(input: String) = {
    val ints  = input.linesIterator.map(_.toInt)
    val part1 = ints.zip(ints.drop(1)).map((x, y) => if y > x then 1 else 0).sum
    val part2 = ints.sliding(3).map(_.sum).sliding(2).map(l => if l(1) > l(0) then 1 else 0).sum
    (part1, part2)
  }
}
