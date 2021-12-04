package day04

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{ignore, failing}

import scala.annotation.tailrec

// ------------------------------------------------------------------------------
case class ChosenNumbers(numbers: Iterable[Int])

case class Cell(number: Int, marked: Boolean = false)

case class Grid(rows: Iterable[Seq[Cell]]) {
  def play(number: Int): Grid =
    Grid(rows.map(row => row.map(cell => cell.copy(marked = if (number == cell.number) true else cell.marked))))
  def isWin: Boolean          =
    rows.exists(_.forall(_.marked)) ||
      0.until(rows.head.size).exists(pos => rows.forall(row => row.lift(pos).filter(_.marked).isDefined))
}

def parseGrids(lines: List[String]): List[Grid] =
  lines
    .filterNot(_.isBlank)
    .sliding(5, 5)
    .map(gridLines =>
      Grid(
        gridLines
          .map(
            _.trim
              .split("\\s+")
              .toSeq
              .map(value => Cell(value.toInt))
          )
      )
    )
    .toList

def parsePuzzle(input: String): (ChosenNumbers, List[Grid]) =
  input.split("\n").toList match {
    case firstLine :: remaining =>
      (ChosenNumbers(firstLine.trim.split(",").map(_.toInt).toList), parseGrids(remaining))
  }

// ------------------------------------------------------------------------------

@tailrec
def playGame(numbers: Iterable[Int], grids: List[Grid]): Int =
  numbers match {
    case number :: remainingNumbers =>
      val updatedGrids = grids.map(_.play(number))
      updatedGrids.find(_.isWin) match {
        case Some(wonGrid) => number * wonGrid.rows.flatMap(_.filterNot(_.marked).map(_.number)).sum
        case None          => playGame(remainingNumbers, updatedGrids)
      }
  }

def resolveStar1(input: String): Int =
  val (chosenNumbers, grids) = parsePuzzle(input)
  playGame(chosenNumbers.numbers, grids)

// ------------------------------------------------------------------------------

@tailrec
def wonGrids(numbers: Iterable[Int], grids: List[Grid], accu: List[(Int, Grid)] = Nil): List[(Int, Grid)] =
  numbers match {
    case Nil                        => accu
    case number :: remainingNumbers =>
      val (won, notWon) = grids.map(_.play(number)).partition(_.isWin)
      wonGrids(
        remainingNumbers,
        notWon,
        accu ++ won.map(grid => number -> grid)
      )
  }

def resolveStar2(input: String): Int =
  val (chosenNumbers, grids) = parsePuzzle(input)
  val winners                = wonGrids(chosenNumbers.numbers, grids)
  winners.last match {
    case (number, grid) => number * grid.rows.flatMap(_.filterNot(_.marked).map(_.number)).sum
  }

// ------------------------------------------------------------------------------

object Puzzle04Test extends DefaultRunnableSpec {
  val day  = "day04"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 4512) && assertTrue(puzzleResult == 72770)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 1924) && assertTrue(puzzleResult == 13912)
    }
  )
}
