package day01

import helpers.*
import org.junit.runner.RunWith
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore}

object Puzzle {
  def resolveStar1(input: String): Int = {
    input.trim
      .split("\n")
      .map(_.toInt)
      .sliding(2, 1)
      .count { case Array(a, b) => a < b }
  }

  def resolveStar2(input: String): Int = {
    input.trim
      .split("\n")
      .map(_.toInt)
      .sliding(3, 1)
      .toList
      .map(_.sum)
      .sliding(2, 1)
      .count { case List(a, b) => a < b }
  }
}

//@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
object Puzzle01Test extends DefaultRunnableSpec {
  val day  = "day01"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-given-1.txt")
        exampleResult = Puzzle.resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-given-1.txt")
        puzzleResult  = Puzzle.resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 7) && assertTrue(puzzleResult == 1301)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-given-1.txt")
        exampleResult = Puzzle.resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-given-1.txt")
        puzzleResult  = Puzzle.resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 5) && assertTrue(puzzleResult == 1346)
    }
  )
}
