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

case class Vertex[A](content: A):
  override def toString() = content.toString

case class Edge[A](from: Vertex[A], to: Vertex[A]):
  override def toString() = s"$from-$to"

type Graph[A] = Map[Vertex[A], List[Vertex[A]]]

type Path[A]  = List[Vertex[A]]
type Cost=Int

case class PartialPath[A](
  current: List[Vertex[A]],
  visited: Map[Vertex[A], Cost],
  cost:Cost,
)
type PartialPaths[A] = List[PartialPath[A]]

def computePaths[A](
  graph: Graph[A],
  from: Vertex[A],
  to: Vertex[A],
  computeCost: Vertex[A] => Cost
): LazyList[Path[A]] =
  def walk(partialPaths: PartialPaths[A], lowestCostForVertex:Map[Vertex[A],Cost]): LazyList[Path[A]] =
    partialPaths match {
      case Nil => LazyList.empty

      case PartialPath(currentPath, visited, currentCost) :: remainingPartialPaths =>
        currentPath match {
          case currentTo :: path if currentTo == to =>
            val updatedLowest = min(currentCost, lowestCostForVertex(currentTo))
            val updatedLowestMap = lowestCostForVertex+(to->updatedLowest)
            currentPath #:: walk(remainingPartialPaths.filter(_.cost <= updatedLowest), updatedLowestMap)

          case currentFrom :: path =>
            val newPartialPaths:PartialPaths[A] = graph(currentFrom).collect {
              case child if !visited.contains(child) &&
                computeCost(child) + currentCost < lowestCostForVertex(child) =>
              val newCost = computeCost(child) + currentCost
              PartialPath(
                child :: currentPath,
                visited+(child->newCost),
                newCost
              )
            }
            val updatedLowestCostForVertex =
              newPartialPaths.foldLeft(lowestCostForVertex){case (current,partialPath) =>
                val child = partialPath.current.head
                val cost = partialPath.cost
                current+(child->min(cost,current(child)))
              }

            val allPartialPaths =
              (remainingPartialPaths ++ newPartialPaths)
                .filterNot{path =>
                  path.current.exists(vertex => path.visited(vertex) > updatedLowestCostForVertex(vertex) )
                }

            walk(allPartialPaths, updatedLowestCostForVertex)
        }
    }
  val initialLowestCosts = graph.keys.map(vertx => vertx->Int.MaxValue).toMap
  val bootpaths = graph(from).map(child => PartialPath(child :: Nil, Map(child->computeCost(child)), computeCost(child)))
  walk(bootpaths, initialLowestCosts)

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
    grid.lift(y - 1).flatMap(_.lift(x)).map(_ => x -> (y - 1)),
    grid.lift(y + 1).flatMap(_.lift(x)).map(_ => x -> (y + 1)),
    grid.lift(y).flatMap(_.lift(x - 1)).map(_ => (x - 1) -> y),
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
    Vertex(cell) ->
      coordsAroundOf(grid, coord)
        .map(aroundCoord => Vertex(Cell(aroundCoord, riskAt(grid, aroundCoord))))
        .toList
        .sortBy(_.content.risk)
  }.toMap

// ------------------------------------------------------------------------------

def pathCost(path: Path[Cell]): Int =
  println(path.map(_.content.risk).mkString("+"))
  def compute(current: Path[Cell], accu: Int = 0): Int =
    current match {
      case Nil            => accu
      case a :: remaining => compute(remaining, accu + a.content.risk)
    }
  compute(path)

def resolveStar1(input: String): BigInt =
  val grid   = parse(input)
  val graph  = gridToGraph(grid)
  val width  = grid.head.size
  val height = grid.size

  val fromCoord = 0           -> 0
  val toCoord   = (width - 1) -> (height - 1)

  val fromVertex = graph.keys.find(_.content.coord == fromCoord).get // TODO of course dangerous
  val toVertex   = graph.keys.find(_.content.coord == toCoord).get   // TODO of course dangerous
  println(Edge(fromVertex, toVertex))
  val paths      = computePaths[Cell](graph, fromVertex, toVertex, _.content.risk)
  paths.map(pathCost).min

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
        && assertTrue(puzzleResult != BigInt(500))
        && assertTrue(puzzleResult != BigInt(577))
        && assertTrue(puzzleResult == BigInt(462))
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(315)) && assertTrue(puzzleResult == BigInt(0))
    } @@ ignore
  )
}
