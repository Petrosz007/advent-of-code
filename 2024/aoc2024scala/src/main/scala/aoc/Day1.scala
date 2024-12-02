package aoc

import aoc.Solution.*

import scala.language.experimental.namedTuples

class Day1 extends Day:
  val file: String = "day1.txt"
  type ParsedInput = (left: List[Long], right: List[Long])

  def parseInput(input: List[String]): ParsedInput =
    input
      .foldLeft((List.empty, List.empty)) { case ((left, right), line) =>
        val Array(lhs, rhs) = line.split(raw"   ")
        (lhs.toInt +: left, rhs.toInt +: right)
      }

  def part1(input: ParsedInput): Solution =
    input.left.sorted
      .zip(input.right.sorted)
      .map { case (a, b) => Math.abs(a - b) }
      .sum

  def part2(input: ParsedInput): Solution =
    val countInRightList = input.right.groupBy(identity).mapValues(_.length.toLong)
    input.left
      .map(x => x.toLong * countInRightList.getOrElse(x, 0L))
      .sum

  def solve(input: ParsedInput): DaySolution =
    val solution1 = part1(input)
    val solution2 = part2(input)

    (part1 = solution1, part2 = solution2)
