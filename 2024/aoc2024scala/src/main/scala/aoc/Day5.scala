package aoc

import scala.language.experimental.namedTuples

class Day5 extends Day:
  val file = "day5.txt"

  type ParsedInput = List[(original: List[Int], sorted: List[Int])]

  def parseInput(input: List[String]): ParsedInput =
    val Array(rawRules, rawUpdates) = input.mkString("\n").split("\n\n")

    val rules = rawRules.linesIterator
      .map(line =>
        val Array(lhs, rhs) = line.split(raw"\|").map(_.toInt)
        (lhs = lhs, rhs = rhs)
      )
      .toSet

    val ordering = Ordering.fromLessThan[Int]((a, b) => rules.contains((a, b)))

    val updates           = rawUpdates.linesIterator.map(line => line.split(",").map(_.toInt).toList).toList
    val originalAndSorted = updates.map(update => (original = update, sorted = update.sorted(ordering)))

    originalAndSorted

  def part1(input: ParsedInput): Solution =
    input
      .filter { case (original, sorted) => original == sorted }
      .map { case (original, _) => original(original.length / 2) }
      .sum

  def part2(input: ParsedInput): Solution =
    input
      .filter { case (original, sorted) => original != sorted }
      .map { case (_, sorted) => sorted(sorted.length / 2) }
      .sum

  def solve(input: ParsedInput): DaySolution =
    (part1 = part1(input), part2 = part2(input))
