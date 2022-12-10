package days
import collection.mutable.ArrayBuffer

object Day10 extends aoc.Day {

  enum Instruction:
    case NoOp
    case Add(i: Int)

  override def solve(input: String): (Any, Any) =
    var values = ArrayBuffer(1)

    for (line <- input.linesIterator)
      val reg = values.last
      line match
        case "noop"     => values.append(reg)
        case s"addx $i" => values.append(reg, reg + i.toInt)

    val p1 = List(20, 60, 100, 140, 180, 220).map(i => i * values(i - 1)).sum

    var p2 = ArrayBuffer[Char]()
    for (i <- 0 until 6)
      for (j <- 0 until 40)
        val reg = values(40 * i + j)
        p2.append(if (reg - 1 to reg + 1).contains(j) then ' ' else '█')
      p2.append('\n')

    (p1, p2.prepended('\n').mkString)

}
