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

  def partOne = println {
    val grid = parseGrid(data)
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
    val lows = horizontalLows.toSet.intersect(verticalLows.toSet)
    grid.filter(x => lows(x._1)).map(_._2 + 1).sum
    // => 496
  }

  def main(args: Array[String]) = {
    partOne
  }
}
