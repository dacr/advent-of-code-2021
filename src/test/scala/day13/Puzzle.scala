package day13

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
case class Dot(x: Int, y: Int)
case class FoldInstruction(horizontal: Boolean, position: Int)

def parseDot(input: String): Dot =
  input.split(",", 2) match
    case Array(xs, ys) => Dot(xs.toInt, ys.toInt)

def parseFoldInstruction(input: String): FoldInstruction =
  input match
    case s"fold along $a=$p" if a == "x" => FoldInstruction(false, p.toInt)
    case s"fold along $a=$p" if a == "y" => FoldInstruction(true, p.toInt)

def parse(input: String): (Set[Dot], List[FoldInstruction]) =
  val Array(coordsSection, instructionsSection) = input.split("\n\n", 2).map(_.trim)
  val coords                                    = coordsSection.split("\n").map(parseDot)
  val instructions                              = instructionsSection.split("\n").map(parseFoldInstruction)
  (coords.toSet, instructions.toList)

// ------------------------------------------------------------------------------

def paperSize(dots:Set[Dot]):(Int,Int) =
  val maxx=dots.maxBy(dot=>dot.x).x
  val maxy=dots.maxBy(dot=>dot.y).y
  (maxx, maxy)

def render(dots:Set[Dot]):String =
  val (maxx, maxy) = paperSize(dots)
  0.to(maxx).map(x=> 0.to(maxy).map(y=> if dots.contains(Dot(x,y)) then "#" else " ").mkString).mkString("\n")

def foldPaper(dots: Set[Dot], instructions: List[FoldInstruction]): Set[Dot] =
  //println("---------------------")
  //println(render(dots))
  instructions match
    case Nil                                                              => dots
    case FoldInstruction(horizontal, position) :: remaining if horizontal =>
      val up     = dots.filter(_.y < position)
      val down   = dots.filter(_.y > position)
      val folded = down.map(dot => Dot(dot.x, 2 * position - dot.y))
      foldPaper(up ++ folded, remaining)
    case FoldInstruction(horizontal, position) :: remaining               =>
      val left   = dots.filter(_.x < position)
      val right  = dots.filter(_.x > position)
      val folded = right.map(dot => Dot(2 * position - dot.x, dot.y))
      foldPaper(left ++ folded, remaining)

// ------------------------------------------------------------------------------

def resolveStar1(input: String): BigInt =
  val (dots, instrs) = parse(input)
  val folded = foldPaper(dots, instrs.head::Nil)
  folded.size

// ------------------------------------------------------------------------------

def resolveStar2(input: String): BigInt =
  val (dots, instrs) = parse(input)
  val folded = foldPaper(dots, instrs)
  //println(render(folded)) // =>DISPLAY : HKUJGAJZ
  folded.size


// ------------------------------------------------------------------------------

object Puzzle13Test extends DefaultRunnableSpec {
  val day  = "day13"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == BigInt(17)) && assertTrue(puzzleResult == BigInt(621))
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt(16)) && assertTrue(puzzleResult == BigInt(95))
    }
  )
}
