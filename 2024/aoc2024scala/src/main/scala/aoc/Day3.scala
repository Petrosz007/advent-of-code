package aoc

import scala.language.experimental.namedTuples

class Day3 extends Day:
  val file = "day3.txt"

  enum Node:
    case Mul(lhs: Long, rhs: Long)
    case Do
    case Dont
  import Node.*

  type ParsedInput = List[Node]

  def parseInput(input: List[String]): ParsedInput =
    val parseRegex = raw"""(?<do>do\(\))|(?<dont>don't\(\))|(?<mul>mul\((?<lhs>\d+),(?<rhs>\d+)\))""".r
    parseRegex
      .findAllMatchIn(input.mkString)
      .map(m =>
        if m.group("do") != null then Do
        else if m.group("dont") != null then Dont
        else if m.group("mul") != null then Mul(m.group("lhs").toLong, m.group("rhs").toLong)
        else throw new RuntimeException("Didn't find any match group, which is super weird")
      )
      .toList

  def part1(input: ParsedInput): Solution =
    input.map {
      case Mul(lhs, rhs) => lhs * rhs
      case _             => 0
    }.sum

  def part2(input: ParsedInput): Solution =
    input
      .foldLeft((true, 0L)) { case ((enabled, sum), node) =>
        node match
          case Do                       => (true, sum)
          case Dont                     => (false, sum)
          case Mul(lhs, rhs) if enabled => (enabled, sum + lhs * rhs)
          case Mul(_, _) if !enabled    => (enabled, sum)
      }
      ._2

  def solve(input: ParsedInput): DaySolution =
    (part1 = part1(input), part2 = part2(input))
