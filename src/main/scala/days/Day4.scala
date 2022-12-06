package days

object Day4 extends aoc.Day {

  override def solve(input: String): (Any, Any) =
    val pattern = """(\d*)-(\d*),(\d*)-(\d*)""".r
    val parsed = input.linesIterator
      .map(
        _ match
          case pattern(a, b, c, d) => (a.toInt to b.toInt, c.toInt to d.toInt)
      )

    val p1 = parsed
      .map((x, y) => x.containsSlice(y) || y.containsSlice(x))
      .count(identity)

    val p2 = parsed.map((x, y) => x intersect y).filter(_.size != 0).size
    (p1, p2)

}
