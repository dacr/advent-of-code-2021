package day10

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
type Chunk = List[Char]
def parse(input:String):Seq[Chunk]=
  input
    .split("\n")
    .map(_.toList)

// ------------------------------------------------------------------------------

val matches= Map(
    '('->')',
    '['->']',
    '{'->'}',
    '<'->'>',
  )
val opens = matches.keys.toSet
val closes = matches.values.toSet

def closeOf(input:Char) = matches(input)
def isOpen(input:Char):Boolean = opens.contains(input)
def isClose(input:Char):Boolean = closes.contains(input)
def isMatch(left:Char, right:Char):Boolean = isOpen(left) && isClose(right) && closeOf(left)==right

// ------------------------------------------------------------------------------

val errorValues=Map(
  ')'->3,
  ']'->57,
  '}'->1197,
  '>'->25137,
)
def getErrorValue(input:Char):Int = errorValues(input)

def scoreChunk(chunk:Chunk):Int =
  @tailrec
  def expect(remaining:Chunk, openedStack:List[Char]=Nil, score:Int=0):Int =
    remaining match
      case _ if score > 0 => score
      case Nil => score
      case head::tail if isOpen(head) => expect(tail, head::openedStack, 0)
      case head::tail if isClose(head) && openedStack.isEmpty => getErrorValue(head)
      case head::tail if isClose(head) && !isMatch(openedStack.head,head) => getErrorValue(head)
      case head::tail => expect(tail, openedStack.tail, score)
  expect(chunk)


def resolveStar1(input: String): Int =
  val chunks = parse(input)
  chunks
    .map(scoreChunk)
    .sum

// ------------------------------------------------------------------------------

val autoCompleteValues=Map(
  ')'->1,
  ']'->2,
  '}'->3,
  '>'->4,
)

def computeScore(chunk:Chunk):Long =
  chunk.foldLeft(0L)( (score, input) => score * 5L + autoCompleteValues(input))

def autocomplete(chunk:Chunk):Chunk=
  @tailrec
  def worker(remaining:Chunk, opens:Chunk=Nil, closes:Chunk=Nil):Chunk =
    remaining match
      case Nil => closes
      case head::tail if isOpen(head) => worker(tail, head::opens, closeOf(head)::closes)
      case head::tail => worker(tail, opens.tail, closes.tail)
  worker(chunk)

def resolveStar2(input: String): Long =
  val chunks = parse(input)
  val scores =
    chunks
      .filter(chunk => scoreChunk(chunk)==0)
      .map(autocomplete)
      .map(computeScore)
      .sorted
  scores.lift(scores.size/2).getOrElse(0)

// ------------------------------------------------------------------------------

object Puzzle10Test extends DefaultRunnableSpec {
  val day  = "day10"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 26397) && assertTrue(puzzleResult == 374061)
    },
    test("star#2 autocomplete tests") {
      val result1 = autocomplete("[({(<(())[]>[[{[]{<()<>>".toList).mkString
      val result2 = autocomplete("{<[[]]>}<{[{[{[]{()[[[]".toList).mkString
      assertTrue(result1 == "}}]])})]") &&
      assertTrue(result2 == "]]}}]}]}>")
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == 288957L) &&
        assertTrue(puzzleResult > 617930596L) &&
        assertTrue(puzzleResult == 2116639949L)
    }
  )
}
