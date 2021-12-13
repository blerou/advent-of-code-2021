import scala.util.matching.Regex
import scala.io.Source

object Day13 {

  val sample = Source
    .fromResource("day13.sample")
    .getLines
    .toList

  val data = Source
    .fromResource("day13.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val (coords, folds) = parseInput(input)
//    println(s"original:\n${draw(coords)}")
    val newCoords = fold(coords, folds.head)
//    println(s"first fold:\n${draw(newCoords)}")
    newCoords.size
    // =>
  }

  type Pos = (Int, Int)
  type Fold = (String, Int)
  def parseInput(lines: Seq[String]): (Set[Pos], Seq[Fold]) = {
    val coords = lines.takeWhile(_.trim.nonEmpty)
    val folds = lines.drop(coords.size).tail
    (parseCoords(coords).toSet, parseFolds(folds))
  }

  val coordR = "(\\d+),(\\d+)".r
  def parseCoords(coords: Seq[String]): Seq[Pos] =
    coords.map { case coordR(x, y) => (x.toInt, y.toInt) }

  val foldR = "fold along ([xy])=(\\d+)".r
  def parseFolds(folds: Seq[String]): Seq[Fold] =
    folds.map { case foldR(dir, len) => (dir, len.toInt) }

  def fold(coords: Set[Pos], fold: Fold): Set[Pos] = fold match {
    case ("x", xf) =>
      coords.filter(_._1 <= xf) ++
        coords.filter(_._1 > xf).map { case (x, y) =>
          (xf - math.abs(xf - x), y)
        }
    case ("y", yf) =>
      coords.filter(_._2 <= yf) ++
        coords.filter(_._2 > yf).map { case (x, y) =>
          (x, yf - math.abs(yf - y))
        }
  }

  def draw(coords: Set[Pos]): String =
    (0 to coords.map(_._2).max)
      .map { y =>
        (0 to coords.map(_._1).max)
          .map { x =>
            if (coords.contains((x, y))) "#" else "."
          }
          .mkString("")
      }
      .mkString("\n")

  def partTwo = println {
    val input = data
    val (coords, folds) = parseInput(input)
    val newCoords = folds.foldLeft(coords)(fold)
    println(s"first fold:\n${draw(newCoords)}")
    newCoords.size
    // => HKUJGAJZ
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
