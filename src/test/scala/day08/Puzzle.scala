package day08

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
type Digit           = Int
type Size            = Int
type Segment         = Char
type Segments        = Set[Segment]
val segmentsByDigit: Map[Digit, Segments] = Map(
  1 -> "cf",
  7 -> "acf",
  4 -> "bcdf",
  8 -> "abcdefg",
  // ---------------
  2 -> "acdeg",
  3 -> "acdfg",
  5 -> "abdfg",
  // ---------------
  9 -> "abcdfg",
  6 -> "abdefg",
  0 -> "abcefg"
).map((k, v) => k -> v.toSet)

val digitBySegments: Map[Segments, Digit] = segmentsByDigit.map((k, v) => (v, k))

def reduceToCommonSegments(segments: Iterable[Segments]): Segments =
  if (segments.size == 1) segments.head
  else segments.reduce(_ intersect _)

val commonSegmentsBySize: Map[Size, Segments] =
  segmentsByDigit.values
    .groupBy(_.size)
    .map((size, segments) => size -> reduceToCommonSegments(segments))

case class Entry(signals: Set[Segments], output: List[Segments])

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

val uniks: Set[Digit]             = Set(1, 4, 7, 8)
val uniksSegments: Set[Segments]  = uniks.flatMap(segmentsByDigit.get)
val uniksSegmentsSizes: Set[Size] = uniksSegments.map(_.size)


def resolveStar1(input: String): Int =
  val entries = parseEntries(input)
  entries.map { entry =>
    entry.output.count { signal =>
      uniksSegmentsSizes.contains(signal.size)
    }
  }.sum

// ------------------------------------------------------------------------------
val allSegments: Segments        = 'a'.to('g').toSet
type SolutionSpace   = Map[Segment, Segments]
type SolutionMapping = Map[Segment, Segment]
val solutionSpace: SolutionSpace = allSegments.map(segment => segment -> allSegments).toMap

def reduceSolutionSpace(space: SolutionSpace, codedSegmentsOfSameSize: Set[Segments]): SolutionSpace =
  val codedSegments     = codedSegmentsOfSameSize.reduce(_ intersect _)
  val candidateSegments = commonSegmentsBySize(codedSegmentsOfSameSize.head.size)
  val otherSegments     = allSegments -- candidateSegments
  otherSegments.map(segment => segment -> (space(segment).removedAll(codedSegments))).toMap ++
    candidateSegments.map(segment => segment -> (space(segment).intersect(codedSegments))).toMap

def computeEntrySolution(entry: Entry): SolutionMapping =
  val inputs  = entry.signals // ++ entry.output
  val mapping =
    inputs
      .groupBy(_.size)
      .foldLeft(solutionSpace) { case (space, (_, segments)) => reduceSolutionSpace(space, segments) }
      .collect { case (segmentA, segments) => segments.head -> segmentA } // // TODO unsafe !
  mapping

def decode(entry: Entry, mapping: SolutionMapping): Int =
  entry.output
    .map(codedSegments => codedSegments.map(mapping))
    .map(decodedSegments => digitBySegments(decodedSegments))
    .mkString
    .toInt

def resolveStar2(input: String): Int =
  val entries  = parseEntries(input)
  val mappings = entries.map(entry => entry -> computeEntrySolution(entry))
  mappings.map(decode).sum

// ------------------------------------------------------------------------------

object Puzzle08Test extends DefaultRunnableSpec {
  val day  = "day08"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-2.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 26) && assertTrue(puzzleResult == 330)
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        exampleInput2 <- Helpers.readFileContent(s"data/$day-example-2.txt")
        exampleResult2 = resolveStar2(exampleInput2)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == 5353) &&
        assertTrue(exampleResult2 == 61229) &&
        assertTrue(puzzleResult == 1010472)
    }
  )
}
