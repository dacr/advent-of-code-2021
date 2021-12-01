package day1.Puzzle

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore}
import helpers.*
import org.junit.runner.RunWith

object Puzzle {
  def resolveStar1(input:String):Int = {
    input
      .trim
      .split("\n")
      .map(_.toInt)
      .sliding(2,1)
      .collect{ case Array(a,b) => a < b}
      .count(_ == true)
  }

  def resolveStar2(input:String):Int = {
    input
      .trim
      .split("\n")
      .map(_.toInt)
      .sliding(3,1)
      .map(_.sum)
      .toList
      .sliding(2,1)
      .collect{ case List(a,b) => a < b}
      .count(_ == true)
  }
}

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
object PuzzleTest extends DefaultRunnableSpec {
  def spec = suite("puzzle day 1")(
    test("star#1 example") {
      for {
        input <- Helpers.readFileContent("data/day1/star1-input-given-1.txt")
        result = Puzzle.resolveStar1(input)
      } yield assertTrue(result == 7)
    },
    test("star#1 input") {
      for {
        input <- Helpers.readFileContent("data/day1/star1-input-given-2.txt")
        result = Puzzle.resolveStar1(input)
      } yield assertTrue(result == 1301)
    },
    test("star#2 example") {
      for {
        input <- Helpers.readFileContent("data/day1/star2-input-given-1.txt")
        result = Puzzle.resolveStar2(input)
      } yield assertTrue(result == 5)
    },
    test("star#2 input") {
      for {
        input <- Helpers.readFileContent("data/day1/star2-input-given-2.txt")
        result = Puzzle.resolveStar2(input)
      } yield assertTrue(result == 1346)
    },
  )
}
