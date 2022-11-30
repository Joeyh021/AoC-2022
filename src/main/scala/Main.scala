package aoc
@main def AoC(day: Int) = println(
  days
    .lift(day)
    .map { day =>
      val (p1, p2) = day.run(); s"Part 1: $p1\nPart 2: $p2"
    } getOrElse "Day unimplemented!"
)

def days = Vector(day0.Day0)
