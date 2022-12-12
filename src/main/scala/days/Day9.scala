package days

import collection.mutable.{ ArrayBuffer, HashSet }

object Day9 extends aoc.Day {

  extension (point: (Int, Int))
    def +(other: (Int, Int))            = (point._1 + other._1, point._2 + other._2)
    def -(other: (Int, Int))            = (point._1 - other._1, point._2 - other._2)
    def isNotCloseTo(other: (Int, Int)) = math.sqrt((point - other).toList.map(x => x * x).sum) >= 1.4143

  override def solve(input: String): (Any, Any) =

    def run(n: Int) =
      var knots   = ArrayBuffer.fill(n)((0, 0))
      var visited = HashSet((0, 0))
      for (instruction <- input.linesIterator.map(_.split(" ")).map(a => (a(0)(0), a(1).toInt)))
        for (_ <- 1 to instruction._2)
          // move the head one step at a time
          knots(0) = knots.head + (instruction._1 match
            case 'U' => (0, 1)
            case 'D' => (0, -1)
            case 'R' => (1, 0)
            case 'L' => (-1, 0)
            case c   => throw Exception(s"Bad input: $c")
          )
          for (i <- 0 until knots.size - 1)
            // if know too far from one infront, then move it 1 step in direction of difference
            if knots(i + 1) isNotCloseTo knots(i) then
              knots(i + 1) += (knots(i) - knots(i + 1))
                .map[[X] =>> X match { case X => Int }]([T] => (x: T) => x match { case i: Int => i.sign })

          visited.add(knots.last)
      visited.size

    (run(2), run(10))

}
