package day06

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{ignore, failing}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
type State = Vector[Int]
def nextState(state:State):State =
  state
    .map(n => if (n==0) 6 else n-1)
    .appendedAll(state.filter(_==0).map(_ => 8))

def states(initialState:State):LazyList[State]=
  val next = nextState(initialState)
  next#::states(next)

def resolveStar1(input: String): Int =
  val state = input.trim.split(",").map(_.toInt).toVector
  states(state).drop(79).head.size

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle06Test extends DefaultRunnableSpec {
  val day  = "day06"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 5934) && assertTrue(puzzleResult == 365131)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == -1) && assertTrue(puzzleResult == -1)
    }
  )
}
