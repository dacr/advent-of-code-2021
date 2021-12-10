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

def allCoords(cave: Cave): Iterable[Coord] =
  val width  = cave.head.size
  val height = cave.size
  0.until(width).flatMap(x => 0.until(height).map(y => x -> y))

def levelAt(cave: Cave, coord: Coord): Int =
  val (x, y) = coord
  cave(y)(x)

def coordsAroundOf(cave: Cave, coord: Coord): Iterable[Coord] =
  val (x, y) = coord
  Seq(
    cave.lift(y - 1).flatMap(_.lift(x)).map(_ => x -> (y - 1)),
    cave.lift(y + 1).flatMap(_.lift(x)).map(_ => x -> (y + 1)),
    cave.lift(y).flatMap(_.lift(x - 1)).map(_ => (x - 1) -> y),
    cave.lift(y).flatMap(_.lift(x + 1)).map(_ => (x + 1) -> y)
  ).flatten

def render(cave: Cave, selected: Set[Coord]): String =
  val width  = cave.head.size
  val height = cave.size
  0.until(height)
    .map { y =>
      0.until(width)
        .map { x =>
          if selected.contains(x -> y) then levelAt(cave, x -> y).toString else "."
        }
        .mkString
    }
    .mkString("\n")

// ------------------------------------------------------------------------------
def isLowest(cave: Cave, coord: Coord): Boolean =
  val (x, y)           = coord
  val reference        = levelAt(cave, coord)
  val levels = coordsAroundOf(cave, coord).map(coord => levelAt(cave, coord))
  levels.forall(_ > reference)

def lowestCoords(cave:Cave):Iterable[Coord] =
  allCoords(cave)
    .filter(coord => isLowest(cave, coord))


def resolveStar1(input: String): Int =
  val cave = parse(input)
  lowestCoords(cave)
    .map(coord => levelAt(cave, coord))
    .map(_ + 1)
    .sum

// ------------------------------------------------------------------------------

def selectable(cave: Cave, coord: Coord, visited: Seq[Coord], limit:Int): Boolean =
  val reference = levelAt(cave, coord)
  val levels    =
    coordsAroundOf(cave, coord)
      .filterNot(visited.contains)
      .map(coord => levelAt(cave, coord))
  levels.forall(_ > reference) && levelAt(cave, coord) < limit

def basinArea(cave: Cave, from: Coord, limit: Int = 9): List[Coord] =
  @tailrec
  def walk(toVisit: List[Coord], visited: Seq[Coord], accu: List[Coord]): List[Coord] =
    // display(cave, accu.toSet)
    toVisit match
      case Nil => accu
      case head :: remaining if selectable(cave, head, visited, limit)  =>
        val arounds = coordsAroundOf(cave, head).filterNot(remaining.contains).filterNot(accu.contains)
        walk(remaining :++ arounds, visited :+ head, head :: accu)
      case head :: remaining =>
        walk(remaining, visited :+ head, accu)

  val area = walk(List(from), Nil, Nil)
  // display(cave, area.toSet)
  area

def resolveStar2(input: String): Int =
  val cave = parse(input)
  lowestCoords(cave)
    .map(coord => basinArea(cave, coord).size)
    .toList
    .sortBy(-_)
    .take(3)
    .product

// TODO try again using a fixpoints approach ?

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
      } yield assertTrue(exampleResult1 == 1134) &&
        assertTrue(puzzleResult > 1017600) &&
        assertTrue(puzzleResult < 280596750) &&
        assertTrue(puzzleResult == 1263735)
    }
  )
}
