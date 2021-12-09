import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  val sample = Source
    .fromResource("day09.sample")
    .getLines
    .toList
  val data = Source
    .fromResource("day09.txt")
    .getLines
    .toList

  type Pos = (Int, Int)
  type Grid = Map[Pos, Int]

  def parseGrid(input: Seq[String]): Grid = {
    for {
      (line, y) <- input.zipWithIndex
      (n, x) <- line.map(_.toString.toInt).zipWithIndex
    } yield (x, y) -> n
  }.toMap

  def isHorizontalLow(grid: Grid, x: Int, y: Int): Boolean = {
    grid.getOrElse((x - 1, y), 10) > grid((x, y)) &&
    grid.getOrElse((x + 1, y), 10) > grid((x, y))
  }
  def isVerticalLow(grid: Grid, x: Int, y: Int): Boolean = {
    grid.getOrElse((x, y - 1), 10) > grid((x, y)) &&
    grid.getOrElse((x, y + 1), 10) > grid((x, y))
  }

  def lowPoints(grid: Grid): Set[Pos] = {
    val Xa = grid.keys.map(_._1).max
    val Ya = grid.keys.map(_._2).max
    val horizontalLows =
      for {
        y <- 0 to Ya
        x <- 0 to Xa
        if isHorizontalLow(grid, x, y)
      } yield (x, y)
    val verticalLows =
      for {
        y <- 0 to Ya
        x <- 0 to Xa
        if isVerticalLow(grid, x, y)
      } yield (x, y)
    horizontalLows.toSet.intersect(verticalLows.toSet)
  }

  def partOne = println {
    val grid = parseGrid(data)
    val lows = lowPoints(grid)
    grid.filter(x => lows(x._1)).map(_._2 + 1).sum
    // => 496
  }

  def basin(grid: Grid, low: Pos): Set[Pos] = {
    def neighbours(pos: Pos): Set[Pos] = {
      Set(
        (pos._1 - 1, pos._2),
        (pos._1 + 1, pos._2),
        (pos._1, pos._2 - 1),
        (pos._1, pos._2 + 1)
      ).filter(grid.contains)
    }
    @tailrec
    def loop(basin: Set[Pos], candidates: Set[Pos]): Set[Pos] = {
      if (candidates.isEmpty)
        basin
      else {
        val newBasin = basin union candidates
        val newCandidates =
          candidates.flatMap(neighbours).filter(grid(_) < 9) -- newBasin
        loop(newBasin, newCandidates)
      }
    }
    loop(Set.empty, Set(low))
  }

  def partTwo = println {
    val grid = parseGrid(data)
    val lows = lowPoints(grid)
    lows.toList.map(basin(grid, _)).map(_.size).sorted.reverse.take(3).product
    // => 902880
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
