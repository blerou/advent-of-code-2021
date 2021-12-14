import scala.annotation.tailrec
import scala.io.Source

object Day11 {

  val sample = Source
    .fromResource("day11.sample")
    .getLines
    .toList

  val sample2 = Source
    .fromResource("day11.sample2")
    .getLines
    .toList

  val sampleSmall = Source
    .fromResource("day11.sample_small")
    .getLines
    .toList

  val data = Source
    .fromResource("day11.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val grid = parseGrid(input)
    val steps = 100
    val (newGrid, flashes) = (0 until steps).foldLeft((grid, 0L)) {
      case ((grid, flashes), _) =>
//        println(draw(grid))
        val newGrid = step(grid)
        val extraFlashes = newGrid.count(_._2 == 0)
        (newGrid, flashes + extraFlashes)
    }
//    println(draw(newGrid))
    flashes
    // => 1747
  }

  type Octo = (Int, Int)
  type Energy = Int
  type Grid = Map[Octo, Energy]

  def parseGrid(lines: Seq[String]): Grid = {
    val pairs = for {
      (line, y) <- lines.zipWithIndex
      (n, x) <- line.zipWithIndex
    } yield (x, y) -> n.toString.toInt
    pairs.toMap
  }

  def step(grid: Grid): Grid = {
    val improvedGrid = grid.map { case (o, e) => (o, e + 1) }
    val flashedGrid = flashIt(improvedGrid, Set.empty)
    val zerodGrid = flashedGrid.map { case (o, e) => (o, if (e > 9) 0 else e) }
    zerodGrid
  }

  @tailrec
  def flashIt(grid: Grid, flashed: Set[Octo]): Grid = {
    val flashes = grid.filter(_._2 > 9).keys.filterNot(flashed.contains)
    val receivedEnergy = flashes.toList.flatMap(neighbours(grid, _))
    if (receivedEnergy.isEmpty)
      grid
    else {
      flashIt(
        receivedEnergy.foldLeft(grid) { case (result, octo) =>
          result.updatedWith(octo)(_.map(_ + 1))
        },
        flashed ++ flashes
      )
    }
  }

  def neighbours(grid: Grid, octo: Octo): Seq[Octo] = {
    val (x, y) = octo
    List(
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ).filter(grid.contains)
      .filter(grid(_) <= 9)
  }

  def draw(grid: Map[(Int, Int), Int]): String =
    (0 to grid.map(_._1._2).max)
      .map { y =>
        (0 to grid.map(_._1._1).max)
          .map { x =>
            grid.get((x, y)).map(v => if (v > 9) "." else s"$v").getOrElse("?")
          }
          .mkString("")
      }
      .mkString("", "\n", "\n")

  def partTwo = println {
    val input = data
    val grid = parseGrid(input)
    fullFlash(grid, 0)
    // => 505
  }

  @tailrec
  def fullFlash(grid: Grid, steps: Int): Int = {
    if (grid.forall(_._2 == 0))
      steps
    else {
      fullFlash(
        step(grid),
        steps + 1
      )
    }
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
