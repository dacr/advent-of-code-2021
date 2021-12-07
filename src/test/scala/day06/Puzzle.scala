package day06

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
type State = Vector[Int]
def nextState(state: State): State =
  state
    .map(n => if (n == 0) 6 else n - 1)
    .appendedAll(state.filter(_ == 0).map(_ => 8))

def states(initialState: State): LazyList[State] =
  val next = nextState(initialState)
  next #:: states(next) // This is not recursive, this is lazy evaluation

def resolvePuzzle(input: String, rounds: Int): Int =
  val state = input.trim.split(",").map(_.toInt).toVector
  states(state).drop(rounds - 1).head.size

// ------------------------------------------------------------------------------

type RefactoredState = Map[Int, BigInt]
def refactoredStates(state: RefactoredState): LazyList[RefactoredState] =
  val decreased = state.map((k, v) => (k - 1) -> v)
  val generated = decreased.getOrElse(-1, BigInt(0))
  val next      =
    (decreased.removed(-1) +
      (8 -> generated)) +
      (6 -> (decreased.getOrElse(6, BigInt(0)) + decreased.getOrElse(-1, BigInt(0))))
  next #:: refactoredStates(next) // This is not recursive, this is lazy evaluation

def refactoredResolvePuzzle(input: String, rounds: Int): BigInt =
  val state =
    input.trim
      .split(",")
      .map(_.toInt)
      .groupBy(identity)
      .map((k, v) => k -> BigInt(v.size))
  refactoredStates(state).drop(rounds - 1).head.values.sum

// ------------------------------------------------------------------------------

object Puzzle06Test extends DefaultRunnableSpec {
  val day  = "day06"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolvePuzzle(exampleInput, 80)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolvePuzzle(puzzleInput, 80)
      } yield assertTrue(exampleResult == 5934) && assertTrue(puzzleResult == 365131)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = refactoredResolvePuzzle(exampleInput, 80)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = refactoredResolvePuzzle(puzzleInput, 256)
      } yield assertTrue(exampleResult == BigInt("5934")) && assertTrue(puzzleResult == BigInt("1650309278600"))
    } @@ timeout(600.seconds) // Not needed thanks to the refactor ;)
  )
}
