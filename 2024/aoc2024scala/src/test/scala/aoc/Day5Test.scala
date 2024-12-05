package aoc

import aoc.AOCMatchers.*
import org.scalatest.*
import org.scalatest.funspec.*
import org.scalatest.matchers.*

import scala.io.Source

class Day5Test extends AnyFunSpec with should.Matchers:
  val day = Day5()

  it("should solve the example part 1"):
    val input = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
    """

    day should solveExamplePart1(input, 143)

  it("should solve the example part 2"):
    val input = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
    """

    day should solveExamplePart2(input, 123)

  it("should solve the day"):
    day should solveDay(part1 = 4766, part2 = 6257)
