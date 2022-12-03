package days

object Day3 extends aoc.Day {
  override def num: Int = 3

  override def solve(input: Seq[String]): (Any, Any) = {
    val toPriority = (c: Char) => if c.isUpper then c.toInt - 38 else c.toInt - 96

    val p1 = input.map { l =>
      val sets         = l.splitAt(l.length() / 2)
      val intersection = (sets(0) intersect sets(1)).distinct
      toPriority(intersection.head)
    }.sum

    val p2 =
      input
        .grouped(3)
        .map(g => g.foldLeft(((('a' to 'z') ++ ('A' to 'Z'))))((s, acc) => (s intersect acc).distinct))
        .map(s => toPriority(s.head))
        .sum

    (p1, p2)
  }

}
