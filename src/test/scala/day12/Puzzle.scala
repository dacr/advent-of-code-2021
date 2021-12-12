package day12

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

case class Vertex(id: String):
  val isStart = id == "start"
  val isEnd   = id == "end"
  val isBig   = id.forall(_.isUpper) && !isStart && !isEnd
  val isSmall = id.forall(_.isLower) && !isStart && !isEnd

  override def toString() = id

case class Edge(from: Vertex, to: Vertex):
  override def toString() = s"$from-$to"

type Graph = Map[Vertex, List[Vertex]]

val startVertex = Vertex("start")
val endVertex = Vertex("end")

// ------------------------------------------------------------------------------

def parseEdge(input: String): Edge =
  input.split("-", 2) match
    case Array(a, b) => Edge(Vertex(a), Vertex(b))

def parseEdges(input: String): List[Edge] =
  input
    .split("\n")
    .map(parseEdge)
    .toList

def edgesToBiDirGraph(edges: List[Edge]): Graph =
  (edges ++ edges.map(edge => Edge(edge.to, edge.from)))
    .groupBy(_.from)
    .view
    .mapValues(_.map(_.to))
    .toMap

// ------------------------------------------------------------------------------

def paths(graph: Graph) =
  // NOT TAIL REC BUT REWORKED HERE : https://gist.github.com/dacr/69aae783d92ac68105dad9118d5f1b3e
  def walk(from: Vertex, path: List[Edge], smallVisited: Set[Vertex]): List[List[Edge]] =
    if from.isEnd then path::Nil
    else
      val children        = graph(from).filterNot(_.isStart).filterNot(smallVisited)
      val newSmallVisited = smallVisited ++ Option(from).filter(_.isSmall)
      children.flatMap(child => walk(child, Edge(from, child) :: path, newSmallVisited))
  walk(startVertex, Nil, Set.empty)

def resolveStar1(input: String): BigInt =
  val edges = parseEdges(input)
  val graph = edgesToBiDirGraph(edges)
  paths(graph).distinct.size

// ------------------------------------------------------------------------------

def pathsStar2(graph: Graph) =
  // NOT TAIL REC BUT REWORKED HERE : https://gist.github.com/dacr/69aae783d92ac68105dad9118d5f1b3e
  def walk(from: Vertex, path: List[Edge], smallVisited: Set[Vertex], smallException:Vertex, smallExceptionVisitedCount:Int): List[List[Edge]] =
    if from.isEnd then path::Nil
    else
      val children        =
        graph(from)
          .filterNot(_.isStart)
          .filter(child => !smallVisited.contains(child) || (child == smallException && smallExceptionVisitedCount<2))
      val newSmallVisited = smallVisited ++ Option(from).filter(_.isSmall)
      children.flatMap { child =>
        val newSmallExceptionVisitedCount = smallExceptionVisitedCount + (if (child==smallException) 1 else 0)
        walk(child, Edge(from, child) :: path, newSmallVisited, smallException, newSmallExceptionVisitedCount)
      }

  val smallVertices = graph.keys.filter(_.isSmall).toList
  smallVertices.flatMap(smallVertex =>
    walk(startVertex, Nil, Set.empty, smallVertex, 0)
  )

def resolveStar2(input: String): BigInt =
  val edges = parseEdges(input)
  val graph = edgesToBiDirGraph(edges)
  pathsStar2(graph).distinct.size

// ------------------------------------------------------------------------------

object Puzzle12Test extends DefaultRunnableSpec {
  val day  = "day12"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar1(exampleInput1)
        exampleInput2 <- Helpers.readFileContent(s"data/$day-example-2.txt")
        exampleResult2 = resolveStar1(exampleInput2)
        exampleInput3 <- Helpers.readFileContent(s"data/$day-example-3.txt")
        exampleResult3 = resolveStar1(exampleInput3)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(10))
        && assertTrue(exampleResult2 == BigInt(19))
        && assertTrue(exampleResult3 == BigInt(226))
        && assertTrue(puzzleResult == BigInt(5157))
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        exampleInput2 <- Helpers.readFileContent(s"data/$day-example-2.txt")
        exampleResult2 = resolveStar2(exampleInput2)
        exampleInput3 <- Helpers.readFileContent(s"data/$day-example-3.txt")
        exampleResult3 = resolveStar2(exampleInput3)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(36))
        && assertTrue(exampleResult2 == BigInt(103))
        && assertTrue(exampleResult3 == BigInt(3509))
        && assertTrue(puzzleResult == BigInt(144309))
    }
  )
}
