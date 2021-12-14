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
    // =>
  }

  type Elem = String
  type Pair = String
  type Formula = String
  type Rules = Map[Pair, Elem]

  def parseInput(lines: Seq[String]): (Formula, Rules) =
    (lines.head, parseRules(lines.drop(2)))

  val ruleR = "(\\w+) -> (\\w)".r
  def parseRules(lines: Seq[String]): Rules =
    lines.map { case ruleR(pair, el) => pair -> el }.toMap

  def step(formula: Formula, rules: Rules): Formula = {
    formula
      .zip(formula.tail)
      .map { case (a, b) =>
        val el = rules(s"$a$b")
        s"$a$el"
      }
      .mkString("", "", formula.last.toString)
  }

  def formulaValue(formula: String): Int = {
    val elems = formula
      .groupBy(identity)
      .map { case (k, v) => k -> v.length }
      .toList
      .sortBy(_._2)
    println(elems)
    elems.last._2 - elems.head._2
  }

  def main(args: Array[String]) = {
    partOne
  }
}
