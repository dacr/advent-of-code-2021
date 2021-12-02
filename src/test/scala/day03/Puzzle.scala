package day03

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int = {
  ???
}

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int = {
  ???
}

// ------------------------------------------------------------------------------

object Puzzle03Test extends DefaultRunnableSpec {
  val day  = "day03"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 0) && assertTrue(puzzleResult == 0)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 0) && assertTrue(puzzleResult == 0)
    }
  )
}
