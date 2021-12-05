package day04

import scala.annotation.tailrec
import scala.io.Source
import scala.language.implicitConversions


object Day04Solution1 {

  case class Cell(row: Int, col: Int, number: Int, marked: Boolean = false)

  case class BingoBoard(boardData: List[List[Cell]]) {
    def markCell(numberToMark: Int): BingoBoard = {
      BingoBoard(this.boardData.map(_.map(cell => {
        if (cell.number == numberToMark) {
          cell.copy(marked = true)
        } else {
          cell
        }
      })))
    }

    def checkForBingo(): Boolean = {
      val maybeRowSolved = this.boardData.map(_.forall(_.marked == true)).find(_ == true).getOrElse(false)
      val maybeColumnSolved = (0 to 4).toList
        .map(columnIndex => this.boardData.flatten.filter(_.col == columnIndex).forall(_.marked == true))
        .find(_ == true).getOrElse(false)

      if (maybeRowSolved || maybeColumnSolved) true else false
    }

    def calcScore(number: Int): Int = {
      this.boardData.flatten.filter(!_.marked).map(_.number).sum * number
    }

    override def toString: String = {
      this.boardData.map(_.map(_.number).mkString(" ")).mkString("\n")
    }
  }

  object BingoBoard {
    def createBoard(board: List[String]): BingoBoard = {
      val parsedBoardData = board.zipWithIndex.map {
        case (rowData, rowIndex) => rowData.split(' ').filter(_.nonEmpty).toList.zipWithIndex.map {
          case (number, columnIndex) => Cell(rowIndex, columnIndex, number.toInt)
        }
      }

      BingoBoard(parsedBoardData)
    }
  }

  @tailrec
  def solveBoards(randomNumber: Int, remainingRandomNumbers: List[Int], bingoBoards: List[BingoBoard]): Option[(Int, BingoBoard)] = {
    val updatedBoards = bingoBoards.map(_.markCell(randomNumber))
    val foundBingo = updatedBoards.map(board => (board.checkForBingo(), board)).find(_._1 == true)
    if (foundBingo.isEmpty) {
      if (remainingRandomNumbers.nonEmpty) {
        solveBoards(remainingRandomNumbers.head, remainingRandomNumbers.tail, updatedBoards)
      } else {
        None
      }
    } else {
      Some(randomNumber, foundBingo.get._2)
    }
  }

  def main(args: Array[String]): Unit = {
    //    val inputData = Source.fromFile("./input-04.txt").getLines.toList
    val inputData = Source.fromFile("./input-04.txt").getLines.toList

    val randomNumbers = inputData.head.split(",").toList.map(_.toInt) //.sliding(5).toList
    val bingoBoards = inputData.tail.filter(_.nonEmpty).grouped(5).toList.map(BingoBoard.createBoard)

    val solvedResult = solveBoards(randomNumbers.head, randomNumbers.tail, bingoBoards)
    val result = solvedResult.get._1
    val board = solvedResult.get._2

    println(s"Day 4 | Solution Part 1: ${board.calcScore(result)}")
  }

}
