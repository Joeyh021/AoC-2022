package days

object Day2 extends aoc.Day {
  //   A B C
  // X 4 1 7
  // Y 8 5 2
  // Z 3 9 6

  //   A B C
  // X 3 1 2
  // Y 4 5 6
  // Z 8 9 7

  def solve(input: String): (Any, Any) = {
    val t1 = Vector(Vector(4, 1, 7), Vector(8, 5, 2), Vector(3, 9, 6))
    val t2 = Vector(Vector(3, 1, 2), Vector(4, 5, 6), Vector(8, 9, 7))
    val s = (t: Vector[Vector[Int]]) =>
      input.linesIterator.map(i => t(i.split(' ')(1)(0).toInt - 88)(i.split(' ')(0)(0).toInt - 65)).sum
    (s(t1), s(t2))
  }

}
