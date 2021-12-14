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

  type CrabPos = Int
  def parseInput(lines: Seq[String]): Seq[CrabPos] =
    lines.head
      .split(",")
      .map(_.toInt)
      .toList

  def main(args: Array[String]) = {
    partOne
  }
}
