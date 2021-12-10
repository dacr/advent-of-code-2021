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
val values=Map(
  ')'->3,
  ']'->57,
  '}'->1197,
  '>'->25137,
)
val matches= Map(
    '('->')',
    '['->']',
    '{'->'}',
    '<'->'>',
  )

def isOpen(input:Char):Boolean =
  "([{<".contains(input)

def isClose(input:Char):Boolean =
  ")]}>".contains(input)

def closeValue(input:Char):Int = values(input)

def isMatch(left:Char, right:Char):Boolean =
  isOpen(left) && isClose(right) && matches(left)==right

def scoreChunk(chunk:Chunk):Int=
  @tailrec
  def expect(remaining:Chunk, stack:List[Char]=Nil, score:Int=0):Int =
    remaining match
      case _ if score > 0 => score
      case Nil => score
      case head::tail if isOpen(head) => expect(tail, head::stack, 0)
      case head::tail if isClose(head) && stack.isEmpty => closeValue(head)
      case head::tail if isClose(head) && !isMatch(stack.head,head) => closeValue(head)
      case head::tail => expect(tail, stack.tail, score)
  expect(chunk)


def analyze(chunks:Iterable[Chunk])=
  chunks
    .map(scoreChunk)
    .sum

def resolveStar1(input: String): Int =
  val chunks = parse(input)
  analyze(chunks)

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0

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
