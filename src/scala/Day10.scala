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
    data.flatMap(firstIllegalChar(_).map(score)).sum
    // => 339477
  }

  def firstIllegalChar(line: String): Option[Char] = {
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
      ._2
  }

  val score = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  def main(args: Array[String]) = {
    partOne
  }
}
