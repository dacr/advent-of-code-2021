package day09

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

type Cave  = Seq[Seq[Int]]
type Coord = (Int, Int)

def parse(input: String): Cave =
  input
    .split("\n")
    .toSeq
    .map(line => line.toSeq.map(_.toInt - 48))

// ------------------------------------------------------------------------------
def isLowest(cave: Cave, coord: Coord): Boolean =
  val (x, y) = coord
  isLowest(cave, x, y)

def isLowest(cave: Cave, x: Int, y: Int): Boolean =
  val reference        = cave(y)(x)
  val levels: Seq[Int] = (
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
    .map(_ + 1)
    .sum

// ------------------------------------------------------------------------------

def lowCoords(cave: Cave): Iterable[Coord] =
  val width  = cave.head.size
  val height = cave.size
  val coords = 0.until(width).flatMap(x => 0.until(height).map(y => x -> y))
  coords.collect { case (x, y) if isLowest(cave, x, y) => (x, y) }

def levelAt(cave: Cave, coord: Coord): Int =
  val (x, y) = coord
  cave(y)(x)

def aroundsOf(cave: Cave, coord: Coord): Iterable[Coord] =
  val (x, y) = coord
  Seq.empty :++
    cave.lift(y - 1).flatMap(_.lift(x)).map(_ => x -> (y - 1)) :++
    cave.lift(y + 1).flatMap(_.lift(x)).map(_ => x -> (y + 1)) :++
    cave.lift(y).flatMap(_.lift(x - 1)).map(_ => (x - 1) -> y) :++
    cave.lift(y).flatMap(_.lift(x + 1)).map(_ => (x + 1) -> y)

def isUnvisitedLowest(cave: Cave, coord: Coord, visited: Seq[Coord]): Boolean =
  val (x, y)    = coord
  val reference = cave(y)(x)
  val levels    =
    aroundsOf(cave, coord)
      .filterNot(visited.contains)
      .collect { case (x, y) => cave(y)(x) }
  levels.forall(_ > reference)

def display(cave: Cave, selected: Set[Coord]): Unit =
  val width  = cave.head.size
  val height = cave.size
  0.until(height).foreach { y =>
    0.until(width).foreach { x =>
      if selected.contains(x -> y) then print(levelAt(cave, x -> y))
      else print(".")
    }
    println()
  }
  println()

def basinArea(cave: Cave, from: Coord, limit: Int = 9): List[Coord] =
  @tailrec
  def walk(toVisit: List[Coord], visited: Seq[Coord], accu: List[Coord]): List[Coord] =
    // display(cave, accu.toSet)
    toVisit match {
      case Nil => accu

      case head :: remaining if levelAt(cave, head) >= limit =>
        walk(remaining, visited :+ head, accu)

      case head :: remaining if isUnvisitedLowest(cave, head, visited) =>
        val arounds = aroundsOf(cave, head).filterNot(remaining.contains).filterNot(accu.contains)
        walk(remaining :++ arounds, visited :+ head, head :: accu)

      case head :: remaining =>
        walk(remaining, visited :+ head, accu)
    }
  walk(List(from), Nil, Nil)

def resolveStar2(input: String): Long =
  val cave = parse(input)
  val lows = lowCoords(cave)
  lows
    .map(coord => basinArea(cave, coord).size.toLong)
    .toList
    .sortBy(-_)
    .take(3)
    .product

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
      } yield assertTrue(exampleResult1 == 1134L) &&
        assertTrue(puzzleResult > 1017600L) &&
        assertTrue(puzzleResult < 280596750L) &&
        assertTrue(puzzleResult == 1263735L)
    }
  )
}
