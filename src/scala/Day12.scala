import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day12 {

  val sample = Source
    .fromResource("day12.sample")
    .getLines
    .toList

  val sample2 = Source
    .fromResource("day12.sample2")
    .getLines
    .toList

  val sample3 = Source
    .fromResource("day12.sample3")
    .getLines
    .toList

  val data = Source
    .fromResource("day12.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val caves = input.flatMap(parseLine).groupMap(_._1)(_._2)
    @tailrec
    def loop(paths: Set[Trail], candidates: Set[Trail]): Set[Trail] =
      if (candidates.isEmpty)
        paths
      else {
        val (done, ongoing) = candidates
          .flatMap { trail =>
            caves(trail.head)
              .filter { cave =>
                cave.forall(_.isUpper) || !trail.contains(cave)
              }
              .map(_ +: trail)
          }
          .partition(_.head == "end")
        loop(paths ++ done, ongoing)
      }
    loop(Set.empty, caves("start").map(List(_)).toSet).size
    // => 3576
  }

  def partTwo = println {
    val input = data
    val caves = input.flatMap(parseLine).groupMap(_._1)(_._2)
    @tailrec
    def loop(paths: Set[Trail], candidates: Set[Trail]): Set[Trail] =
      if (candidates.isEmpty)
        paths
      else {
        val (done, ongoing) = candidates
          .flatMap { trail =>
            val tc = trail
              .filter(_.forall(_.isLower))
              .groupBy(identity)
              .view
              .mapValues(_.size)
              .toMap
            caves(trail.head)
              .filter { cave =>
                val noTwoVisits = tc.forall { case (cave, cnt) => cnt <= 1 }
                val noVisit = !tc.contains(cave)
                cave.forall(_.isUpper) || noTwoVisits || noVisit
              }
              .map(_ +: trail)
          }
          .partition(_.head == "end")
        loop(paths ++ done, ongoing)
      }
    loop(Set.empty, caves("start").map(List(_)).toSet).size
    // => 84271
  }

  type Cave = String
  type Path = (Cave, Cave)
  type Trail = Seq[Cave]

  val linePattern = "([^-]+)-(.+)".r
  def parseLine(line: String): Set[Path] =
    line match {
      case linePattern(a, b) => Set((a, b), (b, a)).filterNot(_._2 == "start")
      case _                 => Set.empty
    }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
