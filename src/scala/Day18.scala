import java.util.stream.Collectors
import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day18 {

  val sample1 =
    """[1,1]
      |[2,2]
      |[3,3]
      |[4,4]
      |""".stripMargin.lines.collect(Collectors.toList[String]).asScala.toList

  val sample2 =
    """[1,1]
      |[2,2]
      |[3,3]
      |[4,4]
      |[5,5]
      |""".stripMargin.lines
      .collect(Collectors.toList[String])
      .asScala
      .toList

  val sample3 =
    """[1,1]
      |[2,2]
      |[3,3]
      |[4,4]
      |[5,5]
      |[6,6]
      |""".stripMargin.lines
      .collect(Collectors.toList[String])
      .asScala
      .toList

  val sample4 =
    """[[[[4,3],4],4],[7,[[8,4],9]]]
      |[1,1]
      |""".stripMargin.lines
      .collect(Collectors.toList[String])
      .asScala
      .toList

  val sample5 =
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
        |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
        |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
        |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
        |[7,[5,[[3,8],[1,4]]]]
        |[[2,[2,2]],[8,[8,1]]]
        |[2,9]
        |[1,[[[9,3],9],[[9,0],[0,7]]]]
        |[[[5,[7,4]],7],1]
        |[[[[4,2],2],6],[8,7]]
        |""".stripMargin.lines
      .collect(Collectors.toList[String])
      .asScala
      .toList

  val sampleHW = Source.fromResource("day18.sample").getLines.toList

  val data = Source.fromResource("day18.txt").getLines.toList

  def partOne = println {
    val input = data
    val snums = parseInput(input)
    val snum = snums.reduce { case (a, b) => a + b }
    snum.magnitude
    // => 4457
  }

  case class Snum(value: IndexedSeq[Rnum]) {

    def +(other: Snum) = add(other).reduce

    def add(other: Snum) =
      Snum(value.map(_.added) ++ other.value.map(_.added))

    private def magnitudeOne = {
      val deepest = value.map(_.depth).max
      val (leftRN, leftIdx) = value.zipWithIndex.find { case (rn, _) =>
        rn.depth == deepest
      }.get
      val rightIdx = leftIdx + 1
      val rightRN = value(rightIdx)
      val newValue = value.slice(0, leftIdx) ++
        Vector(leftRN.magnitude(rightRN)) ++
        value.slice(rightIdx + 1, value.size)
      Snum(newValue)
    }
    private def magnitudeDone = value.tail.isEmpty
    def magnitude: Int =
      if (magnitudeDone) value.head.value
      else magnitudeOne.magnitude

    def explode: Snum = {
      val newValue = value.zipWithIndex
        .find { case (rn, idx) => rn.depth > 4 }
        .flatMap { case (leftRN, leftIdx) =>
          // always exists as the other side of the pair
          val rightIdx = leftIdx + 1
          val rightRN = value(rightIdx)
          val leftNeighIdx = if (leftIdx > 0) Some(leftIdx - 1) else None
          val rightNeighIdx =
            if (rightIdx == value.size - 1) None else Some(rightIdx + 1)
//          println(s"explode $value")
//          println(s"left: $leftRN @ $leftIdx")
//          println(s"right: $rightRN @ $rightIdx")
          Some(value)
            // update left neighbour if exists
            .flatMap { num =>
              leftNeighIdx
                .map { lni =>
                  num.updated(lni, value(lni).inc(leftRN))
                }
                .orElse(Some(num))
            }
            // update right neighbour if exists
            .flatMap { num =>
              rightNeighIdx
                .map { rni =>
                  num.updated(rni, value(rni).inc(rightRN))
                }
                .orElse(Some(num))
            }
            // zero it out
            .map { num =>
              leftNeighIdx
                .map(lni => num.slice(0, lni + 1))
                .getOrElse(Vector.empty) ++
                Vector(leftRN.zero) ++
                rightNeighIdx
                  .map(rni => num.slice(rni, num.size))
                  .getOrElse(Vector.empty)
            }
        }
        .getOrElse(value)
      Snum(newValue)
    }

    def split: Snum = {
      val newValue =
        value.zipWithIndex
          .find { case (rn, _) => rn.value > 9 }
          .map { case (rn, idx) =>
//            println(s"splits: $value")
//            println(s"$rn @ $idx")
            value.slice(0, idx) ++
              Vector(rn.lowSplit, rn.highSplit) ++
              value.slice(idx + 1, value.size)
          }
          .getOrElse(value)
      Snum(newValue)
    }

    def reduce: Snum = {
      @tailrec
      def loop(originalNumber: Snum, newNumber: Option[Snum]): Snum = {
        if (newNumber.isEmpty)
          originalNumber
        else {
          val newOrig = newNumber.get
          val newerNumber =
            newNumber
              .map(_.explode)
              .flatMap(num => if (num != newOrig) Some(num) else None)
              .orElse(
                newNumber
                  .map(_.split)
                  .flatMap(num => if (num != newOrig) Some(num) else None)
              )
          loop(newNumber.get, newerNumber)
        }
      }
      loop(this, Some(this))
    }

  }

  case class Rnum(value: Int, depth: Int) {
    def added = copy(value, depth + 1)
    def inc(other: Rnum) = copy(value + other.value, depth)
    def zero = copy(0, depth - 1)
    def lowSplit = copy(value / 2, depth + 1)
    def highSplit = copy(value - value / 2, depth + 1)
    def magnitude(right: Rnum) = Rnum(3 * value + 2 * right.value, depth - 1)
  }
  def parseInput(lines: Seq[String]): Seq[Snum] = {
    lines.map(toSnum)
  }

  def toSnum(line: String): Snum = {
    val value = line
      .foldLeft((Vector.empty[Rnum], 0)) {
        case ((result, depth), '[') =>
          (result, depth + 1)
        case ((result, depth), ']') =>
          (result, depth - 1)
        case ((result, depth), dig) if dig >= '0' && dig <= '9' =>
          (result :+ Rnum(dig.toString.toInt, depth), depth)
        case (ret, _) =>
          ret
      }
      ._1
    Snum(value)
  }

  def partTwo = println {
    val input = data
    val snums = parseInput(input)
    val magnitudes = for {
      s1 <- snums
      s2 <- snums
      if s1 != s2
    } yield (s1 + s2).magnitude
    magnitudes.max
    // => 4784
  }

  def main(args: Array[String]) = {
    partOne
    partTwo
  }
}
