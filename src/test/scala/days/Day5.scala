package days

import org.scalatest._
import flatspec._
import matchers._
import scala.collection.mutable.ArrayBuffer

class Day5Spec extends AnyFlatSpec {
  "Day 5 Part 1" should "work after step 1" in {
    val stacks = ArrayBuffer(ArrayBuffer('Z', 'N'), ArrayBuffer('M', 'C', 'D'), ArrayBuffer('P'))
    val x      = (1, 2 - 1, 1 - 1)

    assert(Day5.p1(stacks, x) == ArrayBuffer(ArrayBuffer('Z', 'N', 'D'), ArrayBuffer('M', 'C'), ArrayBuffer('P')))
  }

  "Day 5 Part 2" should "work after step 1" in {
    val stacks = ArrayBuffer(ArrayBuffer('Z', 'N'), ArrayBuffer('M', 'C', 'D'), ArrayBuffer('P'))
    val x      = (1, 2 - 1, 1 - 1)

    assert(Day5.p2(stacks, x) == ArrayBuffer(ArrayBuffer('Z', 'N', 'D'), ArrayBuffer('M', 'C'), ArrayBuffer('P')))
  }

  "Day 5 Part 2" should "work after step 2" in {
    var stacks = ArrayBuffer(ArrayBuffer('Z', 'N'), ArrayBuffer('M', 'C', 'D'), ArrayBuffer('P'))
    stacks = Day5.p2(stacks, (1, 2 - 1, 1 - 1))
    stacks = Day5.p2(stacks, (3, 1 - 1, 3 - 1))

    assert(stacks == ArrayBuffer(ArrayBuffer(), ArrayBuffer('M', 'C'), ArrayBuffer('P', 'Z', 'N', 'D')))
  }

  "Day 5 Part 2" should "work after step 3" in {
    var stacks = ArrayBuffer(ArrayBuffer('Z', 'N'), ArrayBuffer('M', 'C', 'D'), ArrayBuffer('P'))
    stacks = Day5.p2(stacks, (1, 2 - 1, 1 - 1))
    stacks = Day5.p2(stacks, (3, 1 - 1, 3 - 1))
    stacks = Day5.p2(stacks, (2, 2 - 1, 1 - 1))

    assert(stacks == ArrayBuffer(ArrayBuffer('M', 'C'), ArrayBuffer(), ArrayBuffer('P', 'Z', 'N', 'D')))
  }
  "Day 5 Part 2" should "work after step 4" in {
    var stacks = ArrayBuffer(ArrayBuffer('Z', 'N'), ArrayBuffer('M', 'C', 'D'), ArrayBuffer('P'))
    stacks = Day5.p2(stacks, (1, 2 - 1, 1 - 1))
    stacks = Day5.p2(stacks, (3, 1 - 1, 3 - 1))
    stacks = Day5.p2(stacks, (2, 2 - 1, 1 - 1))
    stacks = Day5.p2(stacks, (1, 1 - 1, 2 - 1))

    assert(stacks == ArrayBuffer(ArrayBuffer('M'), ArrayBuffer('C'), ArrayBuffer('P', 'Z', 'N', 'D')))
  }

}
