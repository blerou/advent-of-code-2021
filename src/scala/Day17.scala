import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Day17 {

  val sample = "target area: x=20..30, y=-10..-5"

  val data = "target area: x=230..283, y=-107..-57"

  def partOne = println {
    val input = data
    val (x1, x2, y1, y2) = parseInput(input)
    val dy = math.abs(y1) max math.abs(y2)
    (1 until dy).sum
    // => 5671
  }

  val parseR = "^target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)$".r
  def parseInput(line: String) = {
    line match {
      case parseR(x1, x2, y1, y2) =>
        (x1.toInt, x2.toInt, y1.toInt, y2.toInt)
    }
  }

  def main(args: Array[String]) = {
    partOne
  }
}
