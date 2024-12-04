package aoc

import aoc.AOCMatchers.*
import org.scalatest.*
import org.scalatest.funspec.*
import org.scalatest.matchers.*

import scala.io.Source

class Day3Test extends AnyFunSpec with should.Matchers:
  val day = Day3()

  it("should solve the example part 1"):
    val input = """
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
    """

    day should solveExamplePart1(input, 161)

  it("should solve the example part 2"):
    val input = """
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
    """

    day should solveExamplePart2(input, 48)

  it("should solve the day"):
    day should solveDay(part1 = 167_650_499, part2 = 95_846_796)
