package aoc

import scala.io.Source
import scala.language.experimental.namedTuples

enum Solution:
  case Num(x: Long)
  case Printable(s: String)
  case Unsolved

object Solution:
  given fromLongToSolution: Conversion[Long, Solution]     = (x: Long) => Solution.Num(x)
  given fromStringToSolution: Conversion[String, Solution] = (s: String) => Solution.Printable(s)
  given fromUnitToSolution: Conversion[Unit, Solution]     = (_: Unit) => Solution.Unsolved

type DaySolution = (part1: Solution, part2: Solution)

trait Day:
  val file: String
  type ParsedInput

  def parseInput(input: Iterator[String]): ParsedInput
  def part1(input: ParsedInput): Solution
  def part2(input: ParsedInput): Solution
  def solve(input: ParsedInput): DaySolution

  def parsedInput(): ParsedInput =
    val input = Source.fromResource(file).getLines()
    parseInput(input)

  def run(): Unit =
    val solution = solve(parsedInput())

    println(s"""
      |Solution for $file:
      |Part 1: ${solution.part1}
      |Part 2: ${solution.part2}
      |""".stripMargin)
