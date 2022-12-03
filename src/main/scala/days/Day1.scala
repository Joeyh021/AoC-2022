package days

object Day1 extends aoc.Day {

  override def num: Int = 1

  override def solve(input: Seq[String]) = {
    val calories = input
      .foldLeft(List(0)) { (acc, x) =>
        if x == "" then 0 :: acc else acc.head + x.toInt :: acc.tail
      }
    (calories.max, calories.sorted(Ordering[Int].reverse).take(3).sum)
  }
}