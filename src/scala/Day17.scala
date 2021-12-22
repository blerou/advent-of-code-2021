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

  def partTwo = println {
    val input = data
    val (x1, x2, y1, y2) = parseInput(input)
    val maxSteps = (math.abs(y1) max math.abs(y2)) * 2
    // assumes y1 < y2 < 0
    val yCandidates = y1 until -y1
    possibleDxWithSteps(x1, x2).toList.sorted
      .flatMap { case (dx, steps) =>
        yCandidates
          .filter { dy =>
            if (dx > steps) {
              val sum = LazyList.iterate(dy)(_ - 1).take(steps).sum
              y1 <= sum && sum <= y2
            } else {
              LazyList
                .iterate(dy)(_ - 1)
                .take(maxSteps)
                .inits
                .filter(_.size >= steps)
                .exists { s =>
                  val sum = s.sum
                  y1 <= sum && sum <= y2
                }
            }
          }
          .map(dy => (dx, dy))
      }
      .toSet
      .toList
      .sorted
      .size
    // => 4556
  }

  def possibleDxWithSteps(x1: Int, x2: Int) = {
    (1 to (x1 max x2))
      .map { dx =>
        Range
          .inclusive(dx, 0, -1)
          .inits
          .filter { s =>
            val sum = s.sum
            x1 <= sum && sum <= x2
          }
          .map(s => (s.head, s.size))
      }
      .filter(_.nonEmpty)
      .map(_.toSet)
      .reduce(_ union _)
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
