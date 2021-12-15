package day15

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------

case class Vertex[A](id: A):
  override def toString() = id.toString

case class Edge[A](from: Vertex[A], to: Vertex[A]):
  override def toString() = s"$from-$to"

type Graph[A] = Map[Vertex[A], List[Vertex[A]]]
type Path[A]  = List[Edge[A]]

case class PartialPath[A](
  current: Path[A],
  visited: Set[Vertex[A]],
  cost: Double,
  risk: Int
)
type PartialPaths[A] = List[PartialPath[A]]

def computePaths[A](
  graph: Graph[A],
  from: Vertex[A],
  to: Vertex[A],
  revisitable: Vertex[A] => Boolean,
  computeCost: A => Double,
  computeRisk: A => Int
): LazyList[Path[A]] =
  def walk(partialPaths: PartialPaths[A], foundLowestScore:Double): LazyList[Path[A]] =
    partialPaths match {
      case Nil => LazyList.empty

      case PartialPath(currentPath, visited, currentCost, currentRisk) :: remainingPartialPaths =>
        currentPath match {
          case Edge(currentFrom, currentTo) :: path if currentTo == to =>
            val updatedLowest = min(currentCost, foundLowestScore)
            println(s"$currentRisk   ($updatedLowest)")
            currentPath #:: walk(remainingPartialPaths.filter(_.cost < updatedLowest), updatedLowest)

          case Edge(currentFrom, currentTo) :: path =>
            val children        = graph(currentTo).filterNot(_ == currentTo).filterNot(visited)
            val newVisited      = visited ++ Option(currentTo).filterNot(revisitable)
            val newPartialPaths = children.collect { case child if computeCost(currentTo.id) + currentCost < foundLowestScore =>
              PartialPath(
                Edge(currentTo, child) :: currentPath,
                newVisited,
                computeCost(currentTo.id) + currentCost,
                computeRisk(currentTo.id) + currentRisk
              )
            }
            val allPartialPaths =
              (newPartialPaths ++ remainingPartialPaths)
                //.sortBy(partialPath => - partialPath.current.size )
            walk(allPartialPaths, foundLowestScore)
        }
    }

  val bootpaths = graph(from).map(to => PartialPath(Edge(from, to) :: Nil, Set(from), computeCost(to.id), computeRisk(to.id)))
  walk(bootpaths, Double.MaxValue)

// ------------------------------------------------------------------------------
type Coord = (Int, Int)
type Grid  = Seq[Seq[Int]]

def allCoords(grid: Grid): Iterable[Coord] =
  val width  = grid.head.size
  val height = grid.size
  0.until(width).flatMap(x => 0.until(height).map(y => x -> y))

def riskAt(grid: Grid, coord: Coord): Int =
  val (x, y) = coord
  grid(y)(x)

def coordsAroundOf(grid: Grid, coord: Coord): Iterable[Coord] =
  val (x, y) = coord
  Seq(
    //grid.lift(y - 1).flatMap(_.lift(x)).map(_ => x -> (y - 1)),
    grid.lift(y + 1).flatMap(_.lift(x)).map(_ => x -> (y + 1)),
    //grid.lift(y).flatMap(_.lift(x - 1)).map(_ => (x - 1) -> y),
    grid.lift(y).flatMap(_.lift(x + 1)).map(_ => (x + 1) -> y)
  ).flatten

def parse(input: String): Grid =
  input
    .split("\n")
    .map(_.split("").map(_.toInt).toSeq)
    .toSeq

case class Cell(coord: Coord, risk: Int)

def gridToGraph(grid: Grid): Graph[Cell] =
  allCoords(grid).toList.map { coord =>
    val cell = Cell(coord, riskAt(grid, coord))
    Vertex(cell) -> coordsAroundOf(grid, coord).map(aroundCoord => Vertex(Cell(aroundCoord, riskAt(grid, aroundCoord)))).toList
  }.toMap

// ------------------------------------------------------------------------------

def pathCost(path: Path[Cell]): Int =
  def compute(current: Path[Cell], accu: Int = 0): Int =
    current match {
      case Nil            => accu
      case a :: remaining => compute(remaining, accu + a.to.id.risk)
    }
  compute(path)

def cellCost(width: Int, height: Int)(cell: Cell): Double =
  val (x, y) = cell.coord
  //cell.risk
  //10*cell.risk + sqrt(pow((width - x), 2) + pow((height - y), 2))
  10 * cell.risk + 2*(width - x - 1) + 2*(height - y - 1)

def resolveStar1(input: String): BigInt =
  val grid   = parse(input)
  val graph  = gridToGraph(grid)
  val width  = grid.head.size
  val height = grid.size

  val fromCoord = 0           -> 0
  val toCoord   = (width - 1) -> (height - 1)

  val fromVertex = graph.keys.find(_.id.coord == fromCoord).get // TODO of course dangerous
  val toVertex   = graph.keys.find(_.id.coord == toCoord).get   // TODO of course dangerous
  println(Edge(fromVertex, toVertex))
  val paths      = computePaths[Cell](graph, fromVertex, toVertex, vertx => false, cellCost(width, height), _.risk)
  paths.map(pathCost).take(1000).min

// ------------------------------------------------------------------------------

def resolveStar2(input: String): BigInt =
  0

// ------------------------------------------------------------------------------

object Puzzle15Test extends DefaultRunnableSpec {
  val day  = "day15"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == BigInt(40))
        && assertTrue(puzzleResult < BigInt(780))
        && assertTrue(puzzleResult < BigInt(736))
        && assertTrue(puzzleResult < BigInt(500))
        && assertTrue(puzzleResult == BigInt(0))
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(0)) && assertTrue(puzzleResult == BigInt(0))
    } @@ ignore
  )
}
