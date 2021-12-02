package day01

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

def resolveStar1(input: String): Int = {
  input.trim
    .split("\n")
    .toList
    .map(_.toInt)
    .sliding(2, 1)
    .collect { case a::b::_ => a < b }
    .count(identity)
}

def resolveStar2(input: String): Int = {
  input.trim
    .split("\n")
    .map(_.toInt)
    .sliding(3, 1)
    .toList
    .map(_.sum)
    .sliding(2, 1)
    .collect { case a::b::_ => a < b }
    .count(identity)
}

object Puzzle01Test extends DefaultRunnableSpec {
  val day  = "day01"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 7) && assertTrue(puzzleResult == 1301)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 5) && assertTrue(puzzleResult == 1346)
    }
  )
}
