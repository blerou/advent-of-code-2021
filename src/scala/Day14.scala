import scala.io.Source
import scala.util.matching.Regex

object Day14 {

  val sample = Source
    .fromResource("day14.sample")
    .getLines
    .toList

  val data = Source
    .fromResource("day14.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val (formula, rules) = parseInput(input)
    val steps = 10
    val newFormula = (0 until steps).foldLeft(formula) { case (result, _) =>
      step(result, rules)
    }
    formulaValue(newFormula)
    // => 3284
  }

  type Elem = Char
  type Pair = (Char, Char)
  type Formula = Map[Pair, Long]
  type Rules = Map[Pair, Elem]

  def parseInput(lines: Seq[String]): (Formula, Rules) =
    (parseFormula(lines.head), parseRules(lines.drop(2)))

  def parseFormula(line: String): Formula =
    line.zip(line.tail).map(_ -> 1L).toMap

  val ruleR = "(\\w)(\\w) -> (\\w)".r
  def parseRules(lines: Seq[String]): Rules =
    lines.map { case ruleR(a, b, el) => (a.head, b.head) -> el.head }.toMap

  def step(formula: Formula, rules: Rules): Formula = {
    formula
      .foldLeft(Map.empty) { case (result, (pair @ (a, b), n)) =>
        val el = rules(pair)
        result
          .updatedWith((a, el))(_.map(_ + n).orElse(Some(n)))
          .updatedWith((el, b))(_.map(_ + n).orElse(Some(n)))
      }
  }

  def formulaValue(formula: Formula): Long = {
    val elems = formula
      .foldLeft(Map.empty[Elem, Long]) { case (result, ((a, b), n)) =>
        result
          .updatedWith(a)(_.map(_ + n).orElse(Some(n)))
          .updatedWith(b)(_.map(_ + n).orElse(Some(n)))
      }
      .toList
      .sortBy(_._2)
      .map { case (k, v) => (k, v / 2) }
    elems.last._2 - elems.head._2 - 1
  }

  def partTwo = println {
    val input = data
    val (formula, rules) = parseInput(input)
    val steps = 40
    val newFormula = (0 until steps).foldLeft(formula) { case (result, _) =>
      step(result, rules)
    }
    formulaValue(newFormula)
    // => 4302675529689
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
