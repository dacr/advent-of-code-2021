package day03

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{ignore, failing}

import scala.annotation.tailrec

// ------------------------------------------------------------------------------

def flip(inputs: Iterable[String]): Iterable[String] =
  0.until(inputs.head.size).map(pos => inputs.map(input => input(pos)).mkString)

def mostCommon[T](input: Iterable[T]): T =
  input
    .groupBy(identity)
    .map((k, r) => k -> r.size)
    .toList
    .maxBy((k, count) => count) match {
    case (k, _) => k
  }

def leastCommon[T](input: Iterable[T]): T =
  input
    .groupBy(identity)
    .map((k, r) => k -> r.size)
    .toList
    .minBy((k, count) => count) match {
    case (k, _) => k
  }

def resolveStar1(input: String): Int =
  val lines   = input.split("\n")
  val flipped = flip(lines)
  val gamma   = BigInt(flipped.map(mostCommon).mkString, 2)
  val epsilon = BigInt(flipped.map(leastCommon).mkString, 2)
  (gamma * epsilon).toInt

// ------------------------------------------------------------------------------

def extract[T](input: Iterable[Array[T]], pos: Int): Iterable[T] =
  input.flatMap(_.lift(pos))

def search(values: Array[String], onLessOrEqual: Char, onMore: Char, pos: Int = 0): String = {
  val extracted = extract(values.map(_.toCharArray), pos)
  val counters  = extracted.groupBy(identity).view.mapValues(_.size)
  val count0    = counters.getOrElse('0', 0)
  val count1    = counters.getOrElse('1', 0)
  if (count0 <= count1) {
    val filtered = values.filter(value => value.lift(pos) == Some(onLessOrEqual))
    if (filtered.size == 1) filtered.head
    else search(filtered, onLessOrEqual, onMore, pos + 1)
  } else {
    val filtered = values.filter(value => value.lift(pos) == Some(onMore))
    if (filtered.size == 1) filtered.head
    else search(filtered, onLessOrEqual, onMore, pos + 1)
  }
}

def resolveStar2(input: String): Int = {
  val values         = input.split("\n")
  val oxygenRating   = BigInt(search(values, '1', '0'), 2)
  val scrubberRating = BigInt(search(values, '0', '1'), 2)
  (oxygenRating * scrubberRating).toInt
}

// ------------------------------------------------------------------------------

object Puzzle03Test extends DefaultRunnableSpec {
  val day  = "day03"
  def spec = suite(s"puzzle $day")(
    test("flip tools") {
      for {
        result <- ZIO.succeed(flip(List("AB", "CD")))
      } yield assertTrue(result == List("AC", "BD"))
    },
    test("least or most comment item tools") {
      for {
        resultLeast <- ZIO.succeed(leastCommon("00011"))
        resultMost  <- ZIO.succeed(mostCommon("00011"))
      } yield assertTrue(resultLeast == '1') && assertTrue(resultMost == '0')
    },
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 198) && assertTrue(puzzleResult == 3885894)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 230) &&
        assertTrue(puzzleResult != 4418691) &&
        assertTrue(puzzleResult == 4375225)
    }
  )
}
