import scala.io.Source
import scala.util.matching.Regex

object Day05 {

  val sample = Source
    .fromResource("day05.sample")
    .getLines
    .toList
  val data = Source
    .fromResource("day05.txt")
    .getLines
    .toList

  val coords = "(\\d+),(\\d+)\\s+->\\s+(\\d+),(\\d+)".r

  def partOne = println {
    data
      .flatMap { line =>
        line match {
          case coords(_x1, _y1, _x2, _y2) =>
            val (x1, y1, x2, y2) = (_x1.toInt, _y1.toInt, _x2.toInt, _y2.toInt)
            if (x1 == x2) {
              ((y1 min y2) to (y1 max y2)).map(y => (x1, y))
            } else if (y1 == y2) {
              ((x1 min x2) to (x1 max x2)).map(x => (x, y1))
            } else {
              List.empty
            }
          case _ =>
            List.empty
        }
      }
      .groupBy(identity)
      .values
      .count(_.size > 1)
    // => 5145
  }

  def main(args: Array[String]) = {
    partOne
  }
}
