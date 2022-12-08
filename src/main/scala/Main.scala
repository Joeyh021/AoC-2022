package aoc

import java.time.LocalDate

lazy val isDecember22 = LocalDate.now().getYear() == 2022 && LocalDate.now().getMonthValue() == 12
lazy val today        = LocalDate.now().getDayOfMonth()

def runDay(d: Int): Option[String] =
  solvedDays
    .lift(d)
    .map { day =>
      val (p1, p2) = day.run(d); s"Part 1: $p1\nPart 2: $p2"
    }

@main def AoC(args: Int*) = println(
  if args.size > 1 then "Specify a single day to run"
  else if args.isEmpty && !isDecember22 then "Cannot default to current day, specify a day to run"
  else if args.size == 1 then runDay(args.head) getOrElse "That day has not been solved!"
  else runDay(today) getOrElse "You have not solved today's puzzle yet!"
)

import days._
val solvedDays = Vector(Day0, Day1, Day2, Day3, Day4, Day5, Day6, Day7)
