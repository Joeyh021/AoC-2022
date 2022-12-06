package aoc

import scala.util.Using
import scala.io.Source
import java.nio.file.{ Files, Paths }

trait Day:
  def solve(input: String): (Any, Any)

  def run(n: Int): (Any, Any) =
    val path = Paths.get(s"input/$n.txt")
    if Files.exists(path) then Using(Source.fromFile(path.toString))(input => solve(input.mkString)).get
    else solve(Api.downloadInput(n))
