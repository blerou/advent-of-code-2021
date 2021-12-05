package day03

import scala.io.Source

val sample = Source.fromResource("day03.sample").getLines.toList
val data = Source.fromResource("day03.txt").getLines.toList

val partOne = {
  val input = data
  val gamma =
    input
      .foldLeft(Array.fill(input.head.size)(0)) { case (result, line) =>
        line
          .map(bit => if (bit == '1') 1 else 0)
          .zipWithIndex
          .foldLeft(result) { case (result, (bit, pos)) =>
            result(pos) = result(pos) + bit
            result
          }
      }.map { bits =>
        if (bits > input.size / 2) 1 else 0
      }.toList
  val epsilon = gamma.map(bit => if (bit == 1) 0 else 1)
  val result = value(gamma) * value(epsilon)
  println(result)
  // => 4139586
}

def value(l: List[Int]): Int =
  l.reverse.zipWithIndex.foldLeft(0) { case (v, (bit, exp)) =>
    if (bit == 1)
      v + math.pow(2, exp).toInt
    else
      v
  }

@main def main() = {
  partOne
}