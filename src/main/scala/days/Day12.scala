package days

import scala.collection.mutable

object Day12 extends aoc.Day {

  extension (grid: IndexedSeq[IndexedSeq[Char]]) def apply(cs: (Int, Int)) = grid(cs._1)(cs._2)

  def getCoord(c: Char)(using grid: IndexedSeq[IndexedSeq[Char]]) = {
    val row    = grid.indexWhere(_.contains(c))
    val column = grid(row).indexWhere(_ == c)
    (row, column)
  }

  def getNeighbours(up: Boolean)(c: (Int, Int))(using g: IndexedSeq[IndexedSeq[Char]]) = {
    val grid = g.map(_.map(_ match
      case 'S' => 'a'
      case 'E' => 'z'
      case x   => x
    ))

    val (i, j) = c
    List((i, j + 1), (i, j - 1), (i - 1, j), (i + 1, j))
      .filter((i, j) => i >= 0 && i < grid.size && j >= 0 && j < grid(0).size)
      .filter(nc => if up then (grid(nc) <= grid(c) + 1) else (grid(nc) + 1 >= grid(c)))
  }

  def shortestPath(up: Boolean)(from: (Int, Int))(using grid: IndexedSeq[IndexedSeq[Char]]) = {
    val distances = mutable.Map((from, 0))
    val queue     = mutable.Queue[(Int, Int)]()
    val nodes = queue addAll (for {
      i <- 0 until grid.size
      j <- 0 until grid(0).size
    } yield (i, j))

    // dijkstra, badly implemented
    while !(queue.isEmpty) do {
      val v = queue.minBy(distances.getOrElse(_, 9999))
      queue.dequeueAll(_ == v)
      for (n <- getNeighbours(up)(v)) {
        val alt = distances.getOrElse(v, 9999) + 1
        if alt < distances.getOrElse(n, 9999) then distances(n) = alt
      }
    }
    distances
  }

  override def solve(input: String): (Any, Any) = {
    given grid: IndexedSeq[IndexedSeq[Char]] = input.linesIterator.map(_.iterator.toIndexedSeq).toIndexedSeq

    val start = getCoord('S')
    val end   = getCoord('E')

    val p1 = shortestPath(true)(start)(end)
    val p2 = shortestPath(false)(end).filterKeys(grid(_) == 'a').values.toList.min.min(p1)

    (p1, p2)

  }

}
