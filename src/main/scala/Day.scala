package aoc

import scala.util.Using
import scala.io.Source

trait Day {
  def solve(input: String): (Any, Any)

  def run(n: Int): (Any, Any) = Using(Source.fromFile(s"input/$n.txt"))(in => solve(in.toString)).get
}
