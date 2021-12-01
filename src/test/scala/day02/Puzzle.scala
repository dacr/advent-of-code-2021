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
  def spec = suite("puzzle day 2")(
    test("star#1 example") {
      for {
        input <- Helpers.readFileContent("data/day02/star1-input-given-1.txt")
        result = Puzzle.resolveStar1(input)
      } yield assertTrue(result == 0)
    },
    test("star#1 input") {
      for {
        input <- Helpers.readFileContent("data/day02/star1-input-given-2.txt")
        result = Puzzle.resolveStar1(input)
      } yield assertTrue(result == 0)
    },
    test("star#2 example") {
      for {
        input <- Helpers.readFileContent("data/day02/star2-input-given-1.txt")
        result = Puzzle.resolveStar2(input)
      } yield assertTrue(result == 0)
    },
    test("star#2 input") {
      for {
        input <- Helpers.readFileContent("data/day02/star2-input-given-2.txt")
        result = Puzzle.resolveStar2(input)
      } yield assertTrue(result == 0)
    },
  )
}
