package day02

import helpers.*
import org.junit.runner.RunWith
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore}

object Puzzle {
  // ------------------------------------------------------------------------------
  def line2tuple(line: String) =
    line match {
      case s"forward $f" => (f.toInt, 0)
      case s"down $d"    => (0, d.toInt)
      case s"up $d"      => (0, -d.toInt)
    }

  def resolveStar1(input: String): Int = {
    input
      .split("\n")
      .map(line2tuple)
      .reduce { case ((f1, d1), (f2, d2)) => (f1 + f2, d1 + d2) } match { case (f, d) => f * d }
  }

  // ------------------------------------------------------------------------------

  case class Position(horizontal: Int = 0, depth: Int = 0, aim: Int = 0)
  def resolveStar2(input: String): Int = {
    val finalPosition =
      input
        .split("\n")
        .map(line2tuple)
        .foldLeft(Position()) { case (pos, (h, d)) =>
          Position(pos.horizontal + h, pos.depth + pos.aim * h, pos.aim + d)
        }
    finalPosition.depth * finalPosition.horizontal
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
      } yield assertTrue(exampleResult == 150) && assertTrue(puzzleResult == 1693300)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-given-1.txt")
        exampleResult = Puzzle.resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-given-1.txt")
        puzzleResult  = Puzzle.resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 900) && assertTrue(puzzleResult == 1857958050)
    }
  )
}
