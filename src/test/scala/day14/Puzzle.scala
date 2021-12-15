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
  // println(template.mkString+"->"+result.mkString)
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
// REBOOT

// TODO can be simplified !!
case class Pair2(left: Char, right: Char)
case class Count2(count: BigInt, leftCount: BigInt, rightCount: BigInt)
type Rules2      = Map[Pair2, Char]

def reduceTemplate2(template: Map[Pair2, Count2], rules: Rules2): Map[Pair2, Count2] =
  template.toList
    .flatMap { (pair, count) =>
      rules.get(pair) match {
        case None     => (pair -> count) :: Nil
        case Some(ch) =>
          val leftPair  = Pair2(pair.left, ch)
          val rightPair = Pair2(ch, pair.right)
          (leftPair    -> Count2(count.count, count.leftCount, count.count)) ::
            (rightPair -> Count2(count.count, 0, count.rightCount)) :: Nil
      }
    }
    .groupMapReduce((k, v) => k)((k, v) => v) { (a, b) =>
      Count2(
        a.count + b.count,
        a.leftCount + b.leftCount,
        a.rightCount + b.rightCount
      )
    }

def reduceFlow2(template: Map[Pair2, Count2], rules: Rules2): LazyList[Map[Pair2, Count2]] =
  template #:: reduceFlow2(reduceTemplate2(template, rules), rules)

def resolveStar2(input: String, upTo: Int): BigInt =
  val (template, rules) = parse(input)
  val rules2            = rules.collect { case (Seq(l, r), v) => Pair2(l, r) -> v }
  val tuples            =
    template
      .sliding(2, 1)
      .collect { case Seq(l, r) => Pair2(l, r) -> Count2(1, 0, 1) }
      .toList
  val fixedTuples       =
    (tuples.head match { case (k, v) => k -> Count2(1, 1, 1) }) :: tuples.tail

  val flow  = reduceFlow2(fixedTuples.toMap, rules2)
  val found = flow.drop(upTo).head

  val freqs =
    found.toList
      .flatMap { case (pair, count) => (pair.left -> count.leftCount) :: (pair.right -> count.rightCount) :: Nil }
      .groupMapReduce((k, v) => k)((k, v) => v)(_ + _)

  val (mostCommonElement, mostCommonFreq)   = freqs.maxBy { case (c, count) => count }
  val (leastCommonElement, leastCommonFreq) = freqs.minBy { case (c, count) => count }

  mostCommonFreq - leastCommonFreq

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
        exampleResult1 = resolveStar2(exampleInput1, 10)
         exampleResult2 = resolveStar2(exampleInput1,40)
         puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
         puzzleResult   = resolveStar2(puzzleInput, 40)
      } yield assertTrue(exampleResult1 == BigInt(1588))
       && assertTrue(exampleResult2 == BigInt("2188189693529"))
       && assertTrue(puzzleResult == BigInt("3542388214529"))
    }
  )
}
