package aoc

import scala.util.Using
import scala.io.Source

trait Day {
  def num: Int

  def solve(input: Seq[String]): (Any, Any)

  def run(): (Any, Any) = Using(Source.fromFile(s"input/$num.txt"))(input => solve(input.getLines.toSeq)).get
}
