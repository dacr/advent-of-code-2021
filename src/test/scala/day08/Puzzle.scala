package day08

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
type Display = Set[Char]
val segmentsByNumber: Map[Int, Display] = Map(
  0 -> "abcefg",
  1 -> "cf",
  2 -> "acdeg",
  3 -> "acdfg",
  4 -> "bcdf",
  5 -> "abdfg",
  6 -> "abdefg",
  7 -> "acf",
  8 -> "abcdefg",
  9 -> "abcdfg"
).map((k, v) => k -> v.toSet)

val uniks                        = Set(1, 4, 7, 8)
val uniksSegments: Set[Display]  = uniks.flatMap(segmentsByNumber.get)
val uniksSegmentsSizes: Set[Int] = uniksSegments.map(_.size)

case class Entry(signals: Set[Display], output: List[Display])

def parseEntry(input: String): Entry =
  input.split(" [|] ", 2) match {
    case Array(left, right) =>
      Entry(
        signals = left.trim.split(" ").map(_.toSet).toSet,
        output = right.trim.split(" ").map(_.toSet).toList
      )
  }

def parseEntries(input: String): Seq[Entry] =
  input.trim
    .split("\n")
    .toSeq
    .map(parseEntry)

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int =
  val entries = parseEntries(input)
  entries.map { entry =>
    entry.output.count { signal =>
        uniksSegmentsSizes.contains(signal.size)
    }
  }.sum

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0
// ------------------------------------------------------------------------------

object Puzzle08Test extends DefaultRunnableSpec {
  val day  = "day08"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        _            <- ZIO.logInfo(uniksSegmentsSizes.mkString(","))
        exampleInput <- Helpers.readFileContent(s"data/$day-example-2.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 26) && assertTrue(puzzleResult == 330)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-2.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == -1) && assertTrue(puzzleResult == -1)
    } @@ ignore
  )
}
