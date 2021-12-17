package day16

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}
import zio.stream._

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
val hex2bin = Map(
  "0" -> "0000",
  "1" -> "0001",
  "2" -> "0010",
  "3" -> "0011",
  "4" -> "0100",
  "5" -> "0101",
  "6" -> "0110",
  "7" -> "0111",
  "8" -> "1000",
  "9" -> "1001",
  "A" -> "1010",
  "B" -> "1011",
  "C" -> "1100",
  "D" -> "1101",
  "E" -> "1110",
  "F" -> "1111"
)

def toBits(input: String): String = input.split("").map(hex2bin).mkString
def intToBin(n: Int): String  = n.toBinaryString
def binToInt(in: String): Int = java.lang.Integer.parseInt(in, 2)

case class Header(version:Int, typeId:Int)

def decodeHeader(input:String):Header =
  val version = binToInt(input.substring(0,3))
  val typeId = binToInt(input.substring(3, 6))
  Header(version, typeId)



// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int =
  0

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle16Test extends DefaultRunnableSpec {
  val day  = "day16"
  def spec = suite(s"puzzle $day")(
    test("behavior example 1") {
      val input = "D2FE28"
      val bits  = toBits(input)
      val header = decodeHeader(bits)
      assertTrue(bits == "110100101111111000101000") &&
        assertTrue(header.version == 6) &&
        assertTrue(header.typeId == 4)
    },
    test("behavior example 2") {
      val input = "38006F45291200"
      val bits  = toBits(input)
      val header = decodeHeader(bits)
      assertTrue(bits == "00111000000000000110111101000101001010010001001000000000") &&
        assertTrue(header.version == 1) &&
        assertTrue(header.typeId == 6)
    },
    test("behavior example 3") {
      val input = "EE00D40C823060"
      val bits  = toBits(input)
      val header = decodeHeader(bits)
      assertTrue(bits == "11101110000000001101010000001100100000100011000001100000") &&
        assertTrue(header.version == 7) &&
        assertTrue(header.typeId == 3)
    }, // --------------------------------------------
    test("behavior example 4") {
      for {
        input <- ZIO.succeed("8A004A801A8002F478")
        result = resolveStar1(input)
      } yield assertTrue(result == 16)
    },
    test("behavior example 5") {
      for {
        input <- ZIO.succeed("620080001611562C8802118E34")
        result = resolveStar1(input)
      } yield assertTrue(result == 12)
    },
    test("behavior example 6") {
      for {
        input <- ZIO.succeed("C0015000016115A2E0802F182340")
        result = resolveStar1(input)
      } yield assertTrue(result == 23)
    },
    test("behavior example 7") {
      for {
        input <- ZIO.succeed("A0016C880162017C3686B18A3D4780")
        result = resolveStar1(input)
      } yield assertTrue(result == 31)
    }, // --------------------------------------------
    test("star#1") {
      for {
        puzzleInput <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult = resolveStar1(puzzleInput)
      } yield assertTrue(puzzleResult == 0)
    }, // --------------------------------------------
    test("star#2") {
      for {
        puzzleInput <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult = resolveStar2(puzzleInput)
      } yield assertTrue(puzzleResult == 0)
    }
  )
}
