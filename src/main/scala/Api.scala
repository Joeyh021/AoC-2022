package aoc

import sttp.client3._

object Api:
  val backend = HttpClientSyncBackend()
  val response = basicRequest
    .post(uri"https://adventofcode.com/2022/day/$n/input")
    .send(backend)

  println(response.body)
