package day07

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
def parse(input: String):List[Int] =
  input
    .trim
    .split(",")
    .map(_.trim.toInt)
    .toList

def resolveStar1(input: String): Int =
  val positions = parse(input)
  val costs =
    positions.min.to(positions.max).map(goal =>
      positions.map(position => abs(goal - position)).sum
    )
  costs.min

// ------------------------------------------------------------------------------

def moveCost(from:Int, to:Int):Int =
  1.to(abs(to-from)).sum

def resolveStar2(input: String): Int =
  val positions = parse(input)
  val costs =
    positions.min
      .to(positions.max)
      .map(goal =>positions.map(pos => moveCost(pos, goal)).sum)
  costs.min

// ------------------------------------------------------------------------------

object Puzzle07Test extends DefaultRunnableSpec {
  val day  = "day07"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 37) && assertTrue(puzzleResult == 344535)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 168) && assertTrue(puzzleResult == 95581659)
    }
  )
}
