import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Day15 {

  val sample = Source
    .fromResource("day15.sample")
    .getLines
    .toList

  val data = Source
    .fromResource("day15.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val start = Pos(0, 0)
    val grid = parseInput(input)
    val path =
      findPath(
        grid,
        mutable.PriorityQueue(Path(0, List(start))),
        Set.empty,
        grid.keys.max
      )
    println(draw(grid, path))
    path.risk
    // => 621
  }

  @tailrec
  def findPath(
      grid: Grid,
      paths: mutable.PriorityQueue[Path],
      reached: Set[Pos],
      endPos: Pos
  ): Path = {
    val bestPath = paths.dequeue()
    if (bestPath.path.head == endPos) {
      bestPath
    } else {
      val candidates = possibleStepsForward(bestPath)
        .filter(grid.contains)
        .filterNot(reached.contains)
      candidates
        .foreach { pos =>
          val risk = grid(pos)
          paths.enqueue(Path(bestPath.risk + risk, pos +: bestPath.path))
        }
      val newReached = reached ++ candidates.toSet
      findPath(grid, paths, newReached, endPos)
    }
  }

  def possibleStepsForward(path: Path): Seq[Pos] = {
    val pos = path.path.head
    List(Pos(pos.x - 1, pos.y), Pos(pos.x + 1, pos.y), Pos(pos.x, pos.y + 1))
  }

  case class Pos(x: Int, y: Int)
  type Risk = Int
  type Grid = Map[Pos, Risk]
  case class Path(risk: Risk, path: Seq[Pos])

  implicit object PathLowRiskOrdering extends Ordering[Path]() {
    def compare(a: Path, b: Path) = b.risk compare a.risk
  }

  implicit object PosOrdering extends Ordering[Pos] {
    def compare(a: Pos, b: Pos) = {
      val Y = a.y compare b.y
      if (Y == 0) a.x compare b.x else Y
    }
  }

  def parseInput(lines: Seq[String]): Grid = {
    for {
      (line, y) <- lines.zipWithIndex
      (n, x) <- line.zipWithIndex
    } yield Pos(x, y) -> n.toString.toInt
  }.toMap

  def draw(grid: Grid, path: Path): String =
    (0 to grid.keys.map(_._2).max)
      .map { y =>
        (0 to grid.keys.map(_._1).max)
          .map { x =>
            val pos = Pos(x, y)
            if (path.path.contains(pos)) "." else grid(pos).toString
          }
          .mkString("")
      }
      .mkString("", "\n", "\n")

  def main(args: Array[String]) = {
    partOne
  }
}
