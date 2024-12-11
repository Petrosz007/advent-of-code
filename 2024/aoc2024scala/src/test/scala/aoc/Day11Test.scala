package aoc

import aoc.AOCMatchers.*
import org.scalatest.*
import org.scalatest.funspec.*
import org.scalatest.matchers.*

import scala.io.Source

class Day11Test extends AnyFunSpec with should.Matchers:
  val day = Day11()

  it("should solve the example part 1"):
    val input = """
125 17
    """

    day should solveExamplePart1(input, 55312)

  it("should solve the day"):
    day should solveDay(part1 = 218_079, part2 = 259_755_538_429_618L)
