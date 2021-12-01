package day02

import helpers.*
import org.junit.runner.RunWith
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore}

object Puzzle {
  def resolveStar1(input:String):Int = {
    ???
  }

  def resolveStar2(input:String):Int = {
    ???
  }
}

//@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
object Puzzle02Test extends DefaultRunnableSpec {
  val day  = "day02"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-given-1.txt")
        exampleResult = Puzzle.resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-given-1.txt")
        puzzleResult  = Puzzle.resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 0) && assertTrue(puzzleResult == 0)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-given-1.txt")
        exampleResult = Puzzle.resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-given-1.txt")
        puzzleResult  = Puzzle.resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 0) && assertTrue(puzzleResult == 0)
    }
  )
}
