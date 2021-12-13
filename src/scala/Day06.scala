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
    newPopulation.values.sum
    // => 391671
  }

  type Age = Int
  def parseInput(lines: Seq[String]): Map[Age, Long] =
    lines.head
      .split(",")
      .map(_.toInt)
      .groupBy(identity)
      .map { case (age, as) => (age, as.length.toLong) }
      .toMap

  def afterDay(population: Map[Age, Long]): Map[Age, Long] =
    population.foldLeft(Map.empty) { case (result, (age, n)) =>
      if (age > 0)
        result.updatedWith(age - 1)(_.map(_ + n).orElse(Some(n)))
      else
        result
          .updatedWith(6)(_.map(_ + n).orElse(Some(n)))
          .updated(8, n)
    }

  def partTwo = println {
    val input = data
    val population = parseInput(input)
    val days = 256
    val newPopulation = (0 until days).foldLeft(population) { case (pop, _) =>
      afterDay(pop)
    }
    newPopulation.foldLeft(0L)(_ + _._2)
    // => 1754000560399
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
