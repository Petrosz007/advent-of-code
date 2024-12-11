package aoc

import aoc.AOCMatchers.*
import org.scalatest.*
import org.scalatest.funspec.*
import org.scalatest.matchers.*

import scala.io.Source

class Day4Test extends AnyFunSpec with should.Matchers:
  val day = Day4()

  it("should solve the example part 1"):
    val input = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
    """

    day should solveExamplePart1(input, 18)

  it("should solve the example part 2"):
    val input = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
    """

    day should solveExamplePart2(input, 9)

  it("should solve the day"):
    day should solveDay(part1 = 2454, part2 = 1858)
