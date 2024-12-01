package aoc

import AOCMatchers.*

import org.scalatest._
import funspec._
import matchers._
import scala.io.Source
import scala.language.experimental.namedTuples

class Day1Test extends AnyFunSpec with should.Matchers:
  val day = Day1()

  it("should solve the example part 1"):
    val input = """
3   4
4   3
2   5
1   3
3   9
3   3
    """

    day should solveExamplePart1(input, 11L)

  it("should solve the example part 2"):
    val input = """
3   4
4   3
2   5
1   3
3   9
3   3
    """

    day should solveExamplePart2(input, 31L)

  it("should solve the day"):
    day should solveDay(part1 = 3_569_916L, part2 = 26_407_426L)
