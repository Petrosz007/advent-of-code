package aoc

import scala.language.experimental.namedTuples

class Day4 extends Day:
  val file = "day4.txt"
  type Coordinate = (x: Int, y: Int)
  extension (cord: Coordinate) def +(other: Coordinate): Coordinate = (x = cord.x + other.x, y = cord.y + other.y)

  type WordSearch  = Vector[Vector[Char]]
  type ParsedInput = WordSearch

  def parseInput(input: List[String]): WordSearch =
    input.map(_.toVector).toVector

  // format: off
  val directions = Seq(
      (-1, -1), (0, -1), (1, -1),
      (-1,  0),          (1,  0),
      (-1,  1), (0,  1), (1,  1)
    )
  // format: on

  def move(wordSearch: WordSearch, cord: Coordinate, direction: Coordinate): Option[Coordinate] =
    val newCord = (x = cord.x + direction.x, y = cord.y + direction.y)

    if wordSearch.isDefinedAt(newCord.y) && wordSearch(newCord.y).isDefinedAt(newCord.x)
    then Some(newCord)
    else None

  def dfs(wordSearch: WordSearch, cord: Coordinate, direction: Coordinate, remaining: Seq[Char]): Boolean =
    if wordSearch(cord.y)(cord.x) == remaining.head then
      if remaining.tail.isEmpty then true
      else move(wordSearch, cord, direction).exists(newCord => dfs(wordSearch, newCord, direction, remaining.tail))
    else false

  def part1(wordSearch: WordSearch): Solution =
    val coordinates = for
      y <- wordSearch.indices
      x <- wordSearch(y).indices
      if wordSearch(y)(x) == 'X' // Premature optimisation
      direction <- directions
    yield (cord = (x = x, y = y), direction = direction)

    coordinates.count { case (cord, direction) => dfs(wordSearch, cord, direction, "XMAS") }

  def part2(wordSearch: WordSearch): Solution =
    def checkCord(cord: Coordinate, charToCheck: Char): Boolean =
      if wordSearch.isDefinedAt(cord.y) && wordSearch(cord.y).isDefinedAt(cord.x) then
        wordSearch(cord.y)(cord.x) == charToCheck
      else false

    def isXMAS(cord: Coordinate): Boolean =
      Seq("MMSS", "MSMS", "SSMM", "SMSM")
        .map(_.zip(Seq((-1, -1), (1, -1), (-1, 1), (1, 1))))
        .exists(_.forall { case (charToCheck, direction) => checkCord(cord + direction, charToCheck) })

    val coordinates = for
      y <- wordSearch.indices
      x <- wordSearch(y).indices
      if wordSearch(y)(x) == 'A'
    yield (x = x, y = y)

    coordinates.count(isXMAS)

  def solve(input: WordSearch): DaySolution =
    (part1 = part1(input), part2 = part2(input))
