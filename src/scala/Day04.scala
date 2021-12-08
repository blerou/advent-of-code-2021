import scala.annotation.tailrec
import scala.io.Source

object Day04 {

  val sample = Source.fromResource("day04.sample").getLines.toList
  val data = Source.fromResource("day04.txt").getLines.toList

  type Board = Map[Int, (Int, Int)]

  def parseInput(lines: Seq[String]): (Seq[Int], Seq[Board]) = {
    val numbers = lines.head.split(",").toList.map(_.toInt)
    val boards = parseBoards(lines.tail.filterNot(_.trim.isEmpty))
    (numbers, boards)
  }

  @tailrec
  def parseBoards(
      lines: Seq[String],
      boards: Seq[Board] = List.empty
  ): Seq[Board] =
    if (lines.isEmpty)
      boards
    else {
      val (restLines, board) = parseBoard(lines)
      parseBoards(restLines, board +: boards)
    }

  def parseBoard(lines: Seq[String]): (Seq[String], Board) = {
    val board =
      lines
        .take(5)
        .toList
        .map(_.trim.split("[ ,]+").toList.take(5).map(_.toInt))
        .flatten
        .zipWithIndex
        .map { case (n, i) =>
          n -> (i / 5, i % 5)
        }
        .toMap
    (lines.drop(5), board)
  }

  def boardWinning(numbers: Seq[Int], board: Board): Option[Seq[Int]] = {
    numbers.inits.toList.reverse.dropWhile(_.size < 5).foldLeft(Option.empty) {
      case (None, numbers) =>
        val matches = board.filter { case (n, _) => numbers.contains(n) }
        lazy val fullRow =
          matches.map(_._2._1).groupBy(identity).exists { case (_, it) =>
            it.size == 5
          }
        lazy val fullCol =
          matches.map(_._2._2).groupBy(identity).exists { case (_, it) =>
            it.size == 5
          }
        if (fullRow || fullCol) Some(numbers)
        else None
      case (result, _) => result
    }
  }

  def partOne = println {
    val (numbers, boards) = parseInput(data)
    boards
      .flatMap { board =>
        boardWinning(numbers, board) match {
          case Some(numbers) =>
            val nonMatchSum =
              board.filterNot { case (n, _) => numbers.contains(n) }.keys.sum
//          println(nonMatchSum)
//          println(numbers.last)
            Some((numbers.size, nonMatchSum * numbers.last))
          case None => None
        }
      }
      .reduceLeft { case ((r1, s1), (r2, s2)) =>
        if (r1 <= r2) (r1, s1) else (r2, s2)
      }
      ._2
    // => 41503
  }

  def main(args: Array[String]) = {
    partOne
  }
}
