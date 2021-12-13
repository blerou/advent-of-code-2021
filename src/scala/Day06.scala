import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day06 {

  val sample = Source
    .fromResource("day06.sample")
    .getLines
    .toList

  val data = Source
    .fromResource("day06.txt")
    .getLines
    .toList

  def partOne = println {
    val input = data
    val population = parseInput(input)
    val days = 80
    val newPopulation = (0 until days).foldLeft(population) { case (pop, _) =>
      afterDay(pop)
    }
//    println(s"final: $newPopulation")
    newPopulation.size
    // => 391671
  }

  type Age = Int
  def parseInput(lines: Seq[String]): Seq[Age] =
    lines.head.split(",").map(_.toInt).toList

  def afterDay(population: Seq[Age]): Seq[Age] =
    population.flatMap { age =>
      if (age > 0) List(age - 1)
      else List(6, 8)
    }

  def main(args: Array[String]) = {
    partOne
  }
}
