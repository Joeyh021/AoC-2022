package days

import scala.collection.mutable

object Day12 extends aoc.Day {

  extension (grid: IndexedSeq[IndexedSeq[Char]]) def apply(cs: (Int, Int)) = grid(cs._1)(cs._2)

  def getCoord(c: Char)(using grid: IndexedSeq[IndexedSeq[Char]]) = {
    val row    = grid.indexWhere(_.contains(c))
    val column = grid(row).indexWhere(_ == c)
    (row, column)
  }

  def getNeighbours(c: (Int, Int))(using g: IndexedSeq[IndexedSeq[Char]]) = {
    val grid = g.map(_.map(_ match
      case 'S' => 'a'
      case 'E' => 'z'
      case x   => x
    ))

    val (i, j) = c
    List((i, j + 1), (i, j - 1), (i - 1, j), (i + 1, j))
      .filter((i, j) => i >= 0 && i < grid.size && j >= 0 && j < grid(0).size)
      .filter(cs => grid(cs) <= grid(c) + 1)
  }

  def shortestPath(from: (Int, Int), to: (Int, Int))(using grid: IndexedSeq[IndexedSeq[Char]]) = {
    val distances = mutable.Map((from, 0))
    val queue     = mutable.Queue[(Int, Int)]()
    val nodes = queue addAll (for {
      i <- 0 until grid.size
      j <- 0 until grid(0).size
    } yield (i, j))

    // dijkstra, badly implemented
    while !(queue.isEmpty) do {
      val v = queue.minBy(distances.getOrElse(_, Int.MaxValue))
      queue.dequeueAll(_ == v)
      for (n <- getNeighbours(v)) {
        val alt = distances.getOrElse(v, Int.MaxValue) + 1
        if alt < distances.getOrElse(n, Int.MaxValue) then distances(n) = alt
      }
    }
    distances(to)
  }

  override def solve(input: String): (Any, Any) = {
    given grid: IndexedSeq[IndexedSeq[Char]] = input.linesIterator.map(_.iterator.toIndexedSeq).toIndexedSeq

    val start = getCoord('S')
    val end   = getCoord('E')

    val p1 = shortestPath(start, end)

    (p1, 0)

  }

}
