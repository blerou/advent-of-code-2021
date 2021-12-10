import scala.io.Source

object Day10 {

  val sample = Source
    .fromResource("day10.sample")
    .getLines
    .toList
  val data = Source
    .fromResource("day10.txt")
    .getLines
    .toList

  def partOne = println {
    data.flatMap(firstIllegalChar(_).map(illegalScore)).sum
    // => 339477
  }

  type Stack = List[Char]

  def parseLine(line: String): (Stack, Option[Char]) = {
    val pair = Map(
      ')' -> '(',
      ']' -> '[',
      '}' -> '{',
      '>' -> '<'
    )
    line
      .foldLeft((List.empty[Char], Option.empty[Char])) {
        case (result @ (_, Some(_)), _) =>
          result
        case ((stack, None), c) =>
          if (pair.contains(c)) {
            if (pair(c) == stack.head) {
              (stack.tail, None)
            } else {
              (stack, Some(c))
            }
          } else {
            (c +: stack, None)
          }
      }
  }

  def firstIllegalChar(line: String): Option[Char] =
    parseLine(line)._2

  val illegalScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  def partTwo = println {
    val scores = data
      .map(parseLine)
      .flatMap {
        case (stack, None) =>
          Some(autocompleteScoring(stack))
        case (_, Some(_)) =>
          None
      }
      .sorted
    scores(scores.size / 2)
    // => 3049320156
  }

  def autocompleteScoring(stack: Stack): Long =
    stack.foldLeft(0L) { case (result, c) =>
      result * 5 + autocompleteScore(c)
    }

  val autocompleteScore = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4
  )

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
