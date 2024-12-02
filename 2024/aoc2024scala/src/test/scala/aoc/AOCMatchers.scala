package aoc

import org.scalatest._
import matchers._

trait AOCMatchers:
  class DaySolvePart1Matcher(expected: DaySolution) extends Matcher[Day] {

    def apply(day: Day) = {
      val result = day.solve(day.parsedInput())
      MatchResult(
        result == expected,
        s"""${day
            .getClass()
            .getName()} did not solve day correctly, result was $result, expected result: "$expected"""",
        s"""${day.getClass().getName()} solved day correctly, result: "$result""""
      )
    }
  }

  class DaySolvePart1ExampleMatcher(input: String, expected: Solution) extends Matcher[Day] {

    def apply(day: Day) = {
      val result = day.part1(day.parseInput(input.trim().linesIterator.toList))
      MatchResult(
        result == expected,
        s"""${day
            .getClass()
            .getName()} did not solve part1 correctly, result was $result, expected result: "$expected"""",
        s"""${day.getClass().getName()} solved part 1 correctly, result: "$result""""
      )
    }
  }

  class DaySolvePart2ExampleMatcher(input: String, expected: Solution) extends Matcher[Day] {

    def apply(day: Day) = {
      val result = day.part2(day.parseInput(input.trim().linesIterator.toList))
      MatchResult(
        result == expected,
        s"""${day
            .getClass()
            .getName()} did not solve part 2 correctly, result was $result, expected result: "$expected"""",
        s"""${day.getClass().getName()} solved part 2 correctly, result: "$result""""
      )
    }
  }

  def solveDay(expected: DaySolution) =
    new DaySolvePart1Matcher(expected)

  def solveExamplePart1(input: String, expected: Solution) =
    new DaySolvePart1ExampleMatcher(input, expected)

  def solveExamplePart2(input: String, expected: Solution) =
    new DaySolvePart2ExampleMatcher(input, expected)

object AOCMatchers extends AOCMatchers
