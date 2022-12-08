package days

import org.scalatest._
import flatspec._

class Day7Spec extends AnyFlatSpec {

  val testInput = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f.f
2557 g.g
62596 h.lst
$ cd e
$ ls
584 i.i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j.j
8033020 d.log
5626152 d.ext
7214296 k.k
"""

  "Part 1" should "work" in {
    assert(Day7.solve(testInput)._1 == 95437)
  }
}
