package days

import collection.mutable.HashSet

object Day9 extends aoc.Day {

  extension (point: (Int, Int))
    def +(other: (Int, Int))            = (point._1 + other._1, point._2 + other._2)
    def -(other: (Int, Int))            = (point._1 - other._1, point._2 - other._2)
    def isNotCloseTo(other: (Int, Int)) = math.sqrt((point - other).toList.map(x => x * x).sum) >= 1.4143

  def headToTail(h: (Int, Int), t: (Int, Int)): List[(Int, Int)] = {
    var moves = Nil
    var head  = h
    var move  = step(h, t)
    while !move.isEmpty do head = (head._1 + move.get._1, head._2 + move.get._2)

    Nil
  }

  def step(target: (Int, Int), pos: (Int, Int)): Option[(Int, Int)] = {
    def delta = (target._1 - pos._1, target._2 - pos._2)
    if (delta._1 == 1 || delta._1 == 0 || delta._1 == -1) && (delta._2 == 1 || delta._2 == 0 || delta._2 == -1) then
      return None

    delta match
      case (x, 0) => Some((1, 0))
      case (0, x) => Some((0, 1))
      case (x, y) => Some(x.sign, y.sign)
  }

  override def solve(input: String): (Any, Any) =
    var visited = HashSet((0, 0))
    var head    = (0, 0)
    var tail    = (0, 0)
    for (instruction <- input.linesIterator.map(_.split(" ")).map(a => (a(0)(0), a(1).toInt)))
      for (_ <- 1 to instruction._2)
        // move the head one step
        head += (instruction._1 match
          case 'U' => (0, 1)
          case 'D' => (0, -1)
          case 'R' => (1, 0)
          case 'L' => (-1, 0)
          case c   => throw Exception(s"Bad input: $c")
        )
        if head isNotCloseTo tail then
          tail += (head - tail)
            .map[[X] =>> X match { case X => Int }]([T] => (x: T) => x match { case i: Int => i.sign })

        visited.add(tail)

    (visited.size, 0)

}
