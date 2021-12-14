import scala.io.Source

object Day07 {

  val sample = Source
    .fromResource("day07.sample")
    .getLines
    .toList

  val data = Source
    .fromResource("day07.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val crabPos = parseInput(input)
    val bestPos = crabPos.drop(crabPos.size / 2).head
    crabPos.map(p => math.abs(p - bestPos)).sum
    // => 336721
  }

  def partTwo = println {
    val input = data
    val crabPos = parseInput(input)
    val bestPosBase = crabPos.sum * 1.0 / crabPos.size
    fuelTo(crabPos, math.floor(bestPosBase).toInt) min
      fuelTo(crabPos, math.ceil(bestPosBase).toInt)
    // => 91638945
  }

  def fuelTo(crabPos: Seq[CrabPos], candidate: CrabPos): Int =
    crabPos.map(p => (0 to math.abs(p - candidate)).sum).sum

  type CrabPos = Int
  def parseInput(lines: Seq[String]): Seq[CrabPos] =
    lines.head
      .split(",")
      .map(_.toInt)
      .toList

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
