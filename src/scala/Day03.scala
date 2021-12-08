import scala.annotation.tailrec
import scala.io.Source

object Day03 {

  val sample = Source
    .fromResource("day03.sample")
    .getLines
    .toList
    .map(_.map(bit => if (bit == '1') 1 else 0))
  val data = Source
    .fromResource("day03.txt")
    .getLines
    .toList
    .map(_.map(bit => if (bit == '1') 1 else 0))

  def partOne = {
    val input = data
    val gamma =
      input
        .foldLeft(Array.fill(input.head.size)(0)) { case (result, line) =>
          line.zipWithIndex
            .foldLeft(result) { case (result, (bit, pos)) =>
              result(pos) = result(pos) + bit
              result
            }
        }
        .map { bits =>
          if (bits > input.size / 2) 1 else 0
        }
        .toList
    val epsilon = gamma.map(bit => if (bit == 1) 0 else 1)
    val result = value(gamma) * value(epsilon)
    println(result)
    // => 4139586
  }

  def value(l: Seq[Int]): Int =
    l.reverse.zipWithIndex.foldLeft(0) { case (v, (bit, exp)) =>
      if (bit == 1)
        v + math.pow(2, exp).toInt
      else
        v
    }

  def partTwo = {
    val input = data
    @tailrec
    def larger(input: Seq[Seq[Int]], grouping: Int): Seq[Int] = {
      if (input.size == 1)
        input.head
      else {
        val g = input.groupBy(bits => bits(grouping))
        if (g(1).size >= g(0).size) {
          larger(g(1), grouping + 1)
        } else {
          larger(g(0), grouping + 1)
        }
      }
    }
    @tailrec
    def smaller(input: Seq[Seq[Int]], grouping: Int): Seq[Int] = {
      if (input.size == 1)
        input.head
      else {
        val g = input.groupBy(bits => bits(grouping))
        if (g(1).size < g(0).size) {
          smaller(g(1), grouping + 1)
        } else {
          smaller(g(0), grouping + 1)
        }
      }
    }
    val oxygen = larger(input, 0)
    val co2 = smaller(input, 0)

    println(value(oxygen) * value(co2))
    // => 1800151
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
