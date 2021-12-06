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
  val state = input.trim.split(",").map(_.toInt).toVector
  states(state).drop(255).head.size

// ------------------------------------------------------------------------------


type EnhancedState = Map[Int,BigInt]
def statesEnhanced(state:EnhancedState):LazyList[EnhancedState] =
  val decreased:EnhancedState = state.map((k,v)=> (k-1)->v)
  val generated = decreased.getOrElse(-1, BigInt(0))
  val next =
    (decreased.removed(-1) +
      (8-> generated)) +
      (6-> (decreased.getOrElse(6,BigInt(0)) + decreased.getOrElse(-1,BigInt(0))))
  next#::statesEnhanced(next)

def resolveStar2Enhanced(input: String): BigInt =
  val state =
    input
      .trim
      .split(",")
      .map(_.toInt)
      .groupBy(identity)
      .map((k,v) => k->BigInt(v.size))
  statesEnhanced(state).drop(255).head.values.sum



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
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2Enhanced(puzzleInput)
      } yield assertTrue(exampleResult == 5934) && assertTrue(puzzleResult == BigInt("1650309278600",10))
    } @@ timeout(600.seconds)
  )
}
