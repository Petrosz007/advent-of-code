package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParSeq
import scala.collection.{mutable, parallel}

class Day11 extends Day:
  val file = "day11.txt"

  type ParsedInput = Map[Long, Long]

  def parseInput(input: List[String]): ParsedInput =
    input.head.split(' ').map(_.toLong).groupBy(identity).map { case (k, v) => k -> v.length.toLong }

  def doOne(stone: Long): Seq[Long] =
    if stone == 0 then Seq(1)
    else
      val digitCount = Math.log10(stone).floor.longValue + 1
      if digitCount % 2 == 0 then
        val splitter = math.pow(10, digitCount / 2).longValue
        Seq(stone / splitter, stone % splitter)
      else Seq(stone * 2024)

  def blink(stones: Map[Long, Long]): Map[Long, Long] =
    stones.foldLeft(Map.empty[Long, Long]) { case (acc, (stone, count)) =>
      doOne(stone).foldLeft(acc)(_.updatedWith(_)(x => Some(x.getOrElse(0L) + count)))
    }

  def part1(input: ParsedInput): Solution =
    Utils.iterate(25, input)(blink).values.sum

  def part2(input: ParsedInput): Solution =
    Utils.iterate(75, input)(blink).values.sum

  def solve(input: ParsedInput): DaySolution =
    (part1 = part1(input), part2 = part2(input))
