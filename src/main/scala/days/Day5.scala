package days

object Day5 extends aoc.Day:
  type Stacks = Vector[Vector[Char]]

  // the top bit of the input, manually parsed
  val init =
    Vector(
      Vector('H', 'C', 'R'),
      Vector('B', 'J', 'H', 'L', 'S', 'F'),
      Vector('R', 'M', 'D', 'H', 'J', 'T', 'Q'),
      Vector('S', 'G', 'R', 'H', 'Z', 'B', 'J'),
      Vector('R', 'P', 'F', 'Z', 'T', 'D', 'C', 'B'),
      Vector('T', 'H', 'C', 'G'),
      Vector('S', 'N', 'V', 'Z', 'B', 'P', 'W', 'L'),
      Vector('R', 'J', 'Q', 'G', 'C'),
      Vector('L', 'D', 'T', 'R', 'H', 'P', 'F', 'S')
    )

  override def solve(input: Seq[String]): (Any, Any) =
    val pattern = """move (\d*) from (\d*) to (\d*)""".r

    val run = (flip: Boolean) =>
      input
        .map(
          _ match
            case pattern(count, from, to) => (count.toInt, from.toInt - 1, to.toInt - 1)
        )
        .foldLeft(init) { (stacks, x) =>
          val (count, from, to) = x
          stacks
            .updated(
              to,
              stacks(to) ++ (if flip then stacks(from).takeRight(count).reverse
                             else stacks(from).takeRight(count))
            )
            .updated(from, stacks(from).dropRight(count))
        }
        .map(_.last)
        .mkString

    (run(true), run(false))
