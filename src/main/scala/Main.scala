package aoc

@main def AoC(n: Int) = println(
  solvedDays
    .lift(n)
    .map { day =>
      val (p1, p2) = day.run(n); s"Part 1: $p1\nPart 2: $p2"
    } getOrElse "Day unimplemented!"
)

import days._
def solvedDays = Vector(Day0, Day1, Day2, Day3, Day4)
