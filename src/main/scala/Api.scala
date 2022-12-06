package aoc

import sttp.client3._
import java.io.{ File, FileReader, FileWriter }
import scala.util.Using
import scala.util.Using
import scala.io.Source
import java.nio.file.{ Files, Paths }
import scala.sys.env

object Api:
  val cookie  = Using(Source.fromFile("cookie"))(f => f.mkString).get
  val backend = HttpClientSyncBackend()

  def downloadInput(n: Int): String =
    val path = Paths.get(s"input/$n.txt")
    println("Downloading input...")
    basicRequest
      .cookie(
        "session" -> cookie
      )
      .get(uri"https://adventofcode.com/2022/day/$n/input")
      .send(backend)
      .body match
      case Left(error)  => throw Exception("Could not download input")
      case Right(input) => Using(FileWriter(File(s"input/$n.txt")))(i => i.write(input)); input
