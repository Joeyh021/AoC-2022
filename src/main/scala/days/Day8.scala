package days

object Day8 extends aoc.Day:

  override def solve(input: String): (Any, Any) =
    val grid = input.linesIterator.map(_.map(_.toInt - 48).toIndexedSeq).toIndexedSeq

    var p1 = 0
    var p2 = 0
    for (i <- 0 until grid.length)
      for (j <- 0 until grid.head.length)
        val height = grid(i)(j)

        val left  = grid(i).take(j).reverse
        val right = grid(i).drop(j + 1)
        val up    = grid.transpose.toIndexedSeq(j).take(i).reverse
        val down  = grid.transpose.toIndexedSeq(j).drop(i + 1)

        val sides = left :: right :: up :: down :: Nil

        p1 += (if sides.map(_.filter(_ >= height).isEmpty).reduce(_ || _) then 1 else 0)
        p2 = sides.map { l =>
          val s = l.indexWhere(_ >= height); if s == -1 then l.size else s + 1
        }.reduce(_ * _).max(p2)

    (p1, p2)
