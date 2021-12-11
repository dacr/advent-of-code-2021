package day11

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
type Energy = Int
type Coord  = (Int, Int)
type Cells  = Map[Coord, Energy]

def renderEnergy(value: Int): String =
  if value > 9 then "+" else value.toString

def render(cellsMap: Cells): String =
  val ((maxX, maxY), _) = cellsMap.maxBy { case ((x, y), _) => x * y }
  0.to(maxY)
    .map(y => 0.to(maxX).map(x => renderEnergy(cellsMap(x -> y))).mkString)
    .mkString("\n")

def arounds(coord: Coord): Iterable[Coord] =
  coord match
    case (x, y) =>
      -1.to(1)
        .flatMap(dx => -1.to(1).map(dy => (x + dx) -> (y + dy)))
        .filterNot(_ == coord)

@tailrec
def flashes(currentCells: Cells, flashableCoords: List[Coord], flashedCoords: Set[Coord]): (Cells, Set[Coord]) =
  flashableCoords match
    case Nil                => currentCells -> flashedCoords
    case coord :: remaining =>
      val aroundToIncrease = arounds(coord).filter(currentCells.contains)
      val increasedArounds = aroundToIncrease.foldLeft(currentCells) { case (cells, coord) =>
        cells.updated(coord, cells(coord) + 1)
      }
      val newFlashable     =
        increasedArounds
          .filter { case (coord, v) => v > 9 }
          .filterNot { case (coord, _) => flashedCoords.contains(coord) || flashableCoords.contains(coord) }
          .keys
          .toList
      flashes(increasedArounds, newFlashable ++ remaining, flashedCoords + coord)

def nextState(cells: Cells): (Cells, Set[Coord]) =
  val increasedCells =
    cells
      .map { case (coord, value) => coord -> (value + 1) }

  val flashableCoords =
    increasedCells.toList
      .collect { case (c, v) if v > 9 => c }
      .sortBy { case (x, y) => y * 10 + x }

  val (flashedCells, flashedCoords) =
    flashes(increasedCells, flashableCoords, Set.empty)

  val resettedCells = flashedCells.map {
    case (c, v) if v > 9 => c -> 0
    case curr            => curr
  }

  (resettedCells, flashedCoords)

def coords(width: Int, height: Int): Iterable[Coord] =
  0.until(width).flatMap(x => 0.until(height).map(y => x -> y))

def parse(input: String): Cells =
  val values =
    input
      .split("\n")
      .toSeq
      .map(_.split("").map(_.toInt))
  val width  = values.head.size
  val height = values.size
  coords(width, height).zip(values.flatten).toMap

// ------------------------------------------------------------------------------

def resolveStar1(input: String, upToStep: Int = 100): BigInt =
  val cells              = parse(input)
  val (lastState, count) =
    1.to(upToStep)
      .foldRight(cells -> 0) { case (step, (currentState, flashedCount)) =>
        val (newState, flashedCoords) = nextState(currentState)
        newState -> (flashedCount + flashedCoords.size)
      }
  count

// ------------------------------------------------------------------------------

def nextStateOnly(from:Cells):Cells =
  val (to,flashedCoords) = nextState(from)
  to

def resolveStar2(input: String): BigInt =
  val initialState = parse(input)

  def states(state:Cells):LazyList[Cells]=
    state#::states(nextStateOnly(state))

  val found =
    states(initialState)
      .zipWithIndex
      .find{case (state,index) => state.values.forall(_ == 0)}

  found match
    case None => 0
    case Some((state,index)) => index


// ------------------------------------------------------------------------------

object Puzzle11Test extends DefaultRunnableSpec {
  val day  = "day11"
  def spec = suite(s"puzzle $day")(
    test("behavior") {
      val simple =
        """11111
          |19991
          |19191
          |19991
          |11111""".stripMargin
      for {
        simpleExample1 <- ZIO.succeed(simple)
        result1         = resolveStar1(simpleExample1, 2)
      } yield assertTrue(result1 == BigInt(9))
    },
    test("star#1") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar1(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(1656))
        && assertTrue(puzzleResult == BigInt(1655))
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(195))
        && assertTrue(puzzleResult == BigInt(337))
    }
  )
}
