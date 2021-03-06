package day05

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{ignore, failing}

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
case class Coord(x: Int, y: Int)

case class Vent(start: Coord, end: Coord):
  def pointsUsingEnhancedNaive(): Seq[Coord] = // enhanced naive based draw algorithm : https://en.wikipedia.org/wiki/Line_drawing_algorithm
    val dx = end.x - start.x
    val dy = end.y - start.y
    if dx == 0 then start.y.to(end.y, signum(dy)).map(y => Coord(start.x, y))
    else if dy == 0 then start.x.to(end.x, signum(dx)).map(x => Coord(x, start.y))
    else if abs(dx) > abs(dy) then start.x.to(end.x, signum(dx)).map(x => Coord(x, start.y + dy * (x - start.x) / dx))
    else start.y.to(end.y, signum(dy)).map(y => Coord(start.x + dx * (y - start.y) / dy, y))

//  def pointsUsingBresenham():Seq[Coord]= // Bresenham's line algorithm : Bresenham's line algorithm
//    val dx = abs(end.x - start.x)
//    val dy = -abs(end.y - start.y)
//    val sx = signum(end.x - start.x)
//    val sy = signum(end.y - start.y)
//    val err = dx + dy
//    def makePoints(x:Int, y:Int, err:Int):LazyList[Coord] =
//      Coord(x,y) #:: (
//        if (x == end.x && y==end.y) LazyList.empty
//        if (2*err >= dy) makePoints(x+sx, y, err+dy)
//        if (2*err <= dx) makePoints(x, y+sy, err+dx)
//      )
//    makePoints(start.x, start.y, err)
//
  def points(): Seq[Coord] =
    //pointsUsingBresenham()
    pointsUsingEnhancedNaive()


def line2vent(line: String): Vent =
  line.trim match
    case s"$x1,$y1 -> $x2,$y2" => Vent(Coord(x1.toInt, y1.toInt), Coord(x2.toInt, y2.toInt))

def parse(input: String): List[Vent] =
  input
    .split("\n")
    .toList
    .map(line2vent)

// ------------------------------------------------------------------------------
def resolveStar1(input: String): Int =
  val vents = parse(input)
  vents
    .filter(vent => vent.start.x == vent.end.x || vent.start.y == vent.end.y)
    .flatMap(_.points())
    .groupBy(identity)
    .count((coord, sz) => sz.size >= 2)

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  val vents = parse(input)
  vents
    .flatMap(_.points())
    .groupBy(identity)
    .count((coord, sz) => sz.size >= 2)


// ------------------------------------------------------------------------------

object Puzzle05Test extends DefaultRunnableSpec {
  val day  = "day05"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == 5) && assertTrue(puzzleResult == 5280)
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult == 12) && assertTrue(puzzleResult == 16716)
    }
  )
}
