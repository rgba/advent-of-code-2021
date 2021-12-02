package day02

import scala.annotation.tailrec
import scala.io.Source

object Day02Solution1 {

  case class Movement(direction: String, units: Int)
  case class Position(horizontal: Int, vertical: Int)

  @tailrec
  def calcPosition(movements: List[Movement], currentPosition: Position): Position = {
    if (movements.nonEmpty) {
      val newPosition = movements.head match {
        case Movement("forward", units) => currentPosition.copy(horizontal = currentPosition.horizontal + units)
        case Movement("down", units) => currentPosition.copy(vertical = currentPosition.vertical + units)
        case Movement("up", units) => currentPosition.copy(vertical = currentPosition.vertical - units)
        case _ => throw UnsupportedOperationException()
      }
      calcPosition(movements.tail, newPosition)
    } else {
      currentPosition
    }
  }

  def main(args: Array[String]): Unit = {

    val inputData = Source.fromFile("./input-02.txt").getLines.toList
    //    val data = inputData.map(_.split(' ')).map(line => Movement(line(0), line(1).toInt))
    val data = List(
      Movement("forward", 5),
      Movement("down", 5),
      Movement("forward", 8),
      Movement("up", 3),
      Movement("down", 8),
      Movement("forward", 2),
    )

    val finalPosition = calcPosition(data, Position(0, 0))
    println(s"Day 2 | Solution Part 1: ${finalPosition.vertical * finalPosition.horizontal}")
  }
  
}
