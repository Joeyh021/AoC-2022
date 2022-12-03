package days

object Day3 extends aoc.Day {
  extension (c: Char) {
    def toPriority = c.toInt - (if c.isUpper then 38 else 96)
  }

  override def solve(input: Seq[String]): (Any, Any) =
    (
      input
        .map(l => l.splitAt(l.size / 2))
        .map((_ intersect _))
        .map(_.head.toPriority)
        .sum,
      input
        .grouped(3)
        .map(_.reduce(_ intersect _).head.toPriority)
        .sum
    )

}
