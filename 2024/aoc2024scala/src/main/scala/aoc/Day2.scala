package aoc

class Day2 extends Day:
  val file = "day2.txt"
  type ParsedInput = List[List[Long]]

  enum SafetyRating:
    case SafeIncrease
    case SafeDecrease
    case Unsafe

  import SafetyRating.*

  def parseInput(input: List[String]): ParsedInput =
    input.map(_.split(" ").map(_.toLong).toList)

  def transitionSafety(current: Long, next: Long): SafetyRating =
    val transition = next - current
    if 1 to 3 contains transition then SafeIncrease
    else if -3 to -1 contains transition then SafeDecrease
    else Unsafe

  def isSafeReport(report: List[Long]): Boolean =
    val evaluations = report
      .sliding(2)
      .map { case List(current, next) => transitionSafety(current, next) }
      .toSeq
    evaluations.forall(_ == SafeIncrease) || evaluations.forall(_ == SafeDecrease)

  def isSafeReport2(report: List[Long]): Boolean =
    (report +: report.indices.map(i => report.patch(i, Seq.empty, 1)))
      .exists(isSafeReport)

  def part1(input: ParsedInput): Solution =
    input.count(isSafeReport)

  def part2(input: ParsedInput): Solution =
    input.count(isSafeReport2)

  def solve(input: ParsedInput): DaySolution =
    (part1 = part1(input), part2 = part2(input))
