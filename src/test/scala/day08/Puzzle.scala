package day08

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
type Segment = Char
type Display = Set[Segment]
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
val allSegments = 'a'.to('g').toSet
type SolutionSpace = Map[Char,Set[Char]]
def solutions:SolutionSpace = allSegments.map(segment => segment->allSegments).toMap

def focalize(solution:SolutionSpace, signal:Display):SolutionSpace = {
  val num = signal.size match {
    case 2 => Some(1)
    case 4 => Some(4)
    case 3 => Some(7)
    case 7 => Some(8)
    case _ => None
  }
  num match {
    case Some(num) =>
      val numSegments = segmentsByNumber(num)
      val otherSegments  = allSegments -- numSegments
      val updated =
        otherSegments.map(segment => segment -> (solution(segment).removedAll(signal) )).toMap ++
        numSegments.map(segment => segment -> (solution(segment).intersect(signal))).toMap
      println("------------------------------------------------")
      println(signal)
      println(num+" -> "+numSegments+" => "+otherSegments)
      println(updated.toList.sorted.mkString("\n"))
      updated
    case None => solution
  }
}

def resolveStar2(input: String): Int =
  val entries = parseEntries(input)
  val solution =
    entries.map(entry =>
      entry.signals.toList.sortBy(_.size).foldLeft(solutions)( (solution,signal) => focalize(solution,signal))
    )
  println("=========================================")
  println(solution)
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
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        //exampleInput2 <- Helpers.readFileContent(s"data/$day-example-2.txt")
        //exampleResult2 = resolveStar2(exampleInput2)
        //puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        //puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == 5353) //&&
        //assertTrue(exampleResult2 == 61229) &&
        //assertTrue(puzzleResult == -1)
    }
  )
}
