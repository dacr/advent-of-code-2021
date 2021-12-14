package day14

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
type SubTemplate = Seq[Char]
type Template    = Seq[Char]
type Pair        = (SubTemplate, Char)
type Rules       = Map[SubTemplate, Char]
// ------------------------------------------------------------------------------
def parsePair(input: String): Pair =
  input.split(" -> ", 2) match
    case Array(from, to) => (from.toCharArray, to.head)

def parse(input: String): (Template, Rules) =
  input.split("\n\n", 2) match
    case Array(template, remaining) =>
      (template.toCharArray, remaining.split("\n").map(parsePair).toMap)

// ------------------------------------------------------------------------------

def applyRule(rules: Rules, subseq: SubTemplate): Seq[Char] =
  rules.get(subseq) match {
    case None     => subseq
    case Some(ch) => Seq(subseq.head, ch, subseq.last)
  }

def reduceTemplate(template: Template, rules: Rules): Template =
  val result =
    template.head +:
      template
        .sliding(2, 1)
        .flatMap(pair => applyRule(rules, pair).tail)
        .toSeq
  //println(template.mkString+"->"+result.mkString)
  result

def reduceFlow(template: Template, rules: Rules): LazyList[Template] =
  template #:: reduceFlow(reduceTemplate(template, rules), rules)

def resolveStar1(input: String): BigInt =
  val (template, rules)                     = parse(input)
  val flow                                  = reduceFlow(template, rules)
  val found                                 = flow.drop(10).head
  val freqs                                 = found.groupBy(identity).view.mapValues(_.size).toList
  val (mostCommonElement, mostCommonFreq)   = freqs.maxBy { case (c, count) => count }
  val (leastCommonElement, leastCommonFreq) = freqs.minBy { case (c, count) => count }
  mostCommonFreq - leastCommonFreq

// ------------------------------------------------------------------------------



def resolveStar2(input: String): BigInt =
  0

// ------------------------------------------------------------------------------

object Puzzle14Test extends DefaultRunnableSpec {
  val day  = "day14"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(exampleResult == BigInt(1588)) && assertTrue(puzzleResult == BigInt(3230))
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(exampleResult1 == BigInt("2188189693529")) && assertTrue(puzzleResult == BigInt(0))
    }
  )
}
