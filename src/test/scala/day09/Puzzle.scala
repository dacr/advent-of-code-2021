package day09

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

type Cave = Seq[Seq[Int]]

def parse(input: String): Cave =
  input
    .split("\n")
    .toSeq
    .map(line => line.toSeq.map(_.toInt-48))

// ------------------------------------------------------------------------------
def isLowest(cave: Cave, x: Int, y: Int): Boolean =
  val reference        = cave(y)(x)
  val levels: Seq[Int] =(
    Seq.empty :++
      cave.lift(y - 1).flatMap(_.lift(x)) :++
      cave.lift(y + 1).flatMap(_.lift(x)) :++
      cave.lift(y).flatMap(_.lift(x - 1)) :++
      cave.lift(y).flatMap(_.lift(x + 1))
    )
    levels.forall(_ > reference)

def resolveStar1(input: String): Int =
  val cave   = parse(input)
  val width  = cave.head.size
  val height = cave.size
  val coords = 0.until(width).flatMap(x => 0.until(height).map(y => x -> y))
  coords
    .collect { case (x, y) if isLowest(cave, x, y) => cave(y)(x) }
    .tap(println)
    .map(_ + 1)
    .sum

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle09Test extends DefaultRunnableSpec {
  val day  = "day09"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 15) && assertTrue(puzzleResult == 452)
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == -1) && assertTrue(puzzleResult == -1)
    }
  )
}
