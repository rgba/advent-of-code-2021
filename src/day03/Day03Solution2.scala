package day03

import scala.annotation.tailrec
import scala.io.Source
import scala.language.implicitConversions

object Day03Solution2 {
  implicit def convertBooleanToInt(b: Boolean): Int = if (b) 1 else 0

  case class Bits(bits: List[Int]) {

    def +(other: Bits): Bits = Bits(this.bits.zip(other.bits).map(_ + _))

    def mostCommon(bitPosition: Int, totalN: Int): Boolean = {
      if (this.bits(bitPosition) >= totalN - this.bits(bitPosition)) {
        true
      } else {
        false
      }
    }

    def toBitString: String = this.bits.mkString("")

    def toInt: Int = Integer.parseInt(this.toBitString, 2)
  }

  def bitCount(input: List[Bits]): Bits = input.tail.foldLeft(input.head)((acc, bits) => {
    acc + bits
  })

  def filterRatings(bitList: List[Bits], inverter: Boolean => Boolean): List[Bits] = {
    @tailrec
    def filterInternal(bitPosition: Int, bitsToFilter: List[Bits]): List[Bits] = {
      if (bitsToFilter.length > 1) {
        val bitCounts = bitsToFilter.tail.foldLeft(bitsToFilter.head)((acc, bits) => {
          acc + bits
        })
        val result = inverter(bitCounts.mostCommon(bitPosition, bitsToFilter.length)).toInt

        filterInternal(bitPosition + 1, bitsToFilter.filter(_.bits(bitPosition) == result))
      } else {
        bitsToFilter
      }
    }

    filterInternal(0, bitList)
  }

  def main(args: Array[String]): Unit = {
    //    val inputData = Source.fromFile("./input-03.txt").getLines.toList
    val inputData = Source.fromFile("./input-03-test.txt").getLines.toList
    val convertedInputData = inputData.map(_.toList.map(_.asDigit)).map(Bits.apply)

    val oxygenRating = filterRatings(convertedInputData, a => a).head.toInt
    val scrubberRating = filterRatings(convertedInputData, a => !a).head.toInt

    println(s"Day 3 | Solution Part 2: ${oxygenRating * scrubberRating}")
  }

}
