package days

import scala.util.matching._
import scala.collection.mutable.{ ArrayBuffer, HashMap, Stack }

object Day7 extends aoc.Day:

  enum Tree:
    case Dir(name: String, contents: List[Tree])
    case File(size: Int)

  import Tree._

  enum ParsedLine:
    case Cd(name: String)
    case CdUp
    case Ls
    case FileInfo(size: Int, name: String)
    case DirInfo(name: String)

  import ParsedLine._

  def parseLine(line: String) =
    val filepat = "([0-9]+) ([a-z]+.[a-z]+)".r
    line match
      case "$ ls"              => Ls
      case "$ cd .."           => CdUp
      case s"$$ cd $dirname"   => Cd(dirname)
      case s"dir $dirname"     => DirInfo(dirname)
      case filepat(size, name) => FileInfo(size.toInt, name)
      case l                   => throw Exception(s"Could not parse input: $l")

  extension (s: Stack[String]) {
    def toPath: String = s.reverse.foldLeft("")((acc, s) => acc + "/" + s)
  }
  override def solve(input: String): (Any, Any) =
    val parsed =
      input.linesIterator.drop(1).map(parseLine)

    var dirs      = HashMap[String, ArrayBuffer[String | Int]](("/root", ArrayBuffer()))
    var dir_stack = Stack("root")

    for (line <- parsed)
      line match
        case ParsedLine.Cd(name)          => dir_stack.push(name); dirs.addOne((dir_stack.toPath, ArrayBuffer()))
        case ParsedLine.CdUp              => dir_stack.pop
        case ParsedLine.Ls                => ()
        case ParsedLine.FileInfo(size, _) => dirs.get(dir_stack.toPath).get.addOne(size)
        case ParsedLine.DirInfo(name)     => dirs.get(dir_stack.toPath).get.addOne(dir_stack.toPath + "/" + name)

    def getDirSize(name: String): Int =
      dirs
        .get(name)
        .get
        .map(_ match
          case s: String => getDirSize(s)
          case i: Int    => i
        )
        .sum

    val p1        = dirs.map((k, v) => getDirSize(k)).filter(_ <= 100000).sum
    val usedSpace = getDirSize("/root")
    val p2        = dirs.toList.map((k, v) => getDirSize(k)).filter(_ > usedSpace - 40000000).min

    (p1, p2)
