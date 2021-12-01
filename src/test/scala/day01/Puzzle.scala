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
  def spec = suite("puzzle day 1")(
    test("star#1 example") {
      for {
        input <- Helpers.readFileContent("data/day01/star1-input-given-1.txt")
        result = Puzzle.resolveStar1(input)
      } yield assertTrue(result == 7)
    },
    test("star#1 input") {
      for {
        input <- Helpers.readFileContent("data/day01/star1-input-given-2.txt")
        result = Puzzle.resolveStar1(input)
      } yield assertTrue(result == 1301)
    },
    test("star#2 example") {
      for {
        input <- Helpers.readFileContent("data/day01/star2-input-given-1.txt")
        result = Puzzle.resolveStar2(input)
      } yield assertTrue(result == 5)
    },
    test("star#2 input") {
      for {
        input <- Helpers.readFileContent("data/day01/star2-input-given-2.txt")
        result = Puzzle.resolveStar2(input)
      } yield assertTrue(result == 1346)
    }
  )
}
