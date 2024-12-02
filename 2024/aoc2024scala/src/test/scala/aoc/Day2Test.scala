package aoc

import AOCMatchers.*

import org.scalatest._
import funspec._
import matchers._
import scala.io.Source

class Day2Test extends AnyFunSpec with should.Matchers:
  val day = Day2()

  it("should solve the example part 1"):
    val input = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
    """

    day should solveExamplePart1(input, 2)

  it("should solve the example part 2"):
    val input = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
    """

    day should solveExamplePart2(input, 4)

  it("should solve the day"):
    day should solveDay(part1 = 559, part2 = 601)
