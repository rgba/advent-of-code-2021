package day03

import scala.annotation.tailrec
import scala.io.Source
import scala.language.implicitConversions

object Day03Solution1 {

  implicit def convertBooleanToInt(b: Boolean): Int = if (b) 1 else 0

  case class Bits(bits: List[Int]) {

    def +(other: Bits): Bits = Bits(this.bits.zip(other.bits).map(_ + _))

    def mostCommon(bit: Int, totalN: Int): Boolean = {
      if (bit > totalN - bit) {
        true
      } else {
        false
      }
    }

    def asGammaRateBits(totalN: Int): Bits = Bits(this.bits.map(bit => mostCommon(bit, totalN)))

    def asEpsilonRateBits(totalN: Int): Bits = Bits(this.bits.map(bit => !mostCommon(bit, totalN)))

    def toBitString(): String = this.bits.mkString("")

    def powerConsumption(totalN: Int): Int =
      Integer.parseInt(this.asGammaRateBits(totalN).toBitString(), 2) *
        Integer.parseInt(this.asEpsilonRateBits(totalN).toBitString(), 2)

  }

  def main(args: Array[String]): Unit = {
    //    val inputData = Source.fromFile("./input-03.txt").getLines.toList
    val inputData = Source.fromFile("./input-03.txt").getLines.toList
    val convertedInputData = inputData.map(_.toList.map(_.asDigit)).map(Bits.apply)

    val bitCount = convertedInputData.tail.foldLeft(convertedInputData.head)((acc, bits) => {
      acc + bits
    })

    println(s"Day 3 | Solution Part 1: ${bitCount.powerConsumption(convertedInputData.length)}")
  }

}
