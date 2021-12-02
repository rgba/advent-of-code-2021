package day01

import scala.annotation.tailrec

object Day01Solution {

  @tailrec
  def slidingWindow(data: List[Int], acc: List[Int]): List[Int] = {
    if (data.length > 3) {
      slidingWindow(data.tail, acc :+ data.take(3).sum)
    } else {
      acc :+ data.take(3).sum
    }
  }

  @tailrec
  def createTuples(data: List[Int], tuples: List[(Int, Int)]): List[(Int, Int)] = {
    if (data.length > 2) {
      createTuples(data.tail, tuples :+ (data.head, data.tail.head))
    } else {
      tuples :+ (data.head, data.tail.head)
    }
  }

  def checkMeasurements(data: List[(Int, Int)]): Int =
    data.foldLeft(0) {
      case (acc, (a, b)) =>
        if (a < b) {
          acc + 1
        } else {
          acc
        }
    }

  def main(args: Array[String]): Unit = {
    //    val data = Source.fromFile("./input-01.txt").getLines.map(_.toInt).toList
    val data = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

    val resultPart1 = checkMeasurements(createTuples(data, List()))
    println(s"Day 1 | Solution Part 1: $resultPart1")

    val resultPart2 = checkMeasurements(createTuples(slidingWindow(data, List()), List()))
    println(s"Day 1 | Solution Part 2: $resultPart2")
  }
  
}
