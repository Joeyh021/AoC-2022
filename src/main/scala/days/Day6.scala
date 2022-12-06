package days

object Day6 extends aoc.Day:
  override def solve(input: String): (Any, Any) =
    val f = (l: Int) => input.iterator.sliding(l).map(s => s.distinct.size == s.size).zipWithIndex.find(_._1).get._2 + l
    (f(4), f(14))
