package day2kotlin

import java.io.File

enum class InstructionType {
    FORWARD, DOWN, UP;

    companion object {
        fun parse(str: String): InstructionType = when(str) {
            "forward" -> InstructionType.FORWARD
            "down"    -> InstructionType.DOWN
            else      -> InstructionType.UP
        }
    }
}

data class Instruction(val instruction: InstructionType, val value: Long) {
    companion object {
        fun parse(line: String): Instruction {
            val (instruction, value) = line.split(' ')
            return Instruction(InstructionType.parse(instruction), value.toLong())
        }
    }
}

data class Position(val horizontal: Long, val depth: Long, val aim: Long) {
    val solution1 = this.horizontal * this.aim
    val solution2 = this.horizontal * this.depth
}

fun getFilePath(fileName: String): String =
    object {}.javaClass.getResource("/$fileName").toString().removePrefix("file:")

fun readInput(fileName: String): List<Instruction> =
    File(getFilePath(fileName))
        .readLines()
        .map { Instruction.parse(it) }


fun execute(instruction: Instruction, pos: Position): Position = 
    when(instruction.instruction) {
        InstructionType.FORWARD -> Position(pos.horizontal + instruction.value, pos.depth + pos.aim * instruction.value, pos.aim)
        InstructionType.DOWN    -> Position(pos.horizontal, pos.depth, pos.aim + instruction.value)
        InstructionType.UP      -> Position(pos.horizontal, pos.depth, pos.aim - instruction.value)
    }

fun solve(input: List<Instruction>): Position =
    input.fold(Position(0, 0, 0)) { pos, instruction -> execute(instruction, pos) }

fun solve1(input: List<Instruction>): Long = solve(input).solution1

fun solve2(input: List<Instruction>): Long = solve(input).solution2

fun main() {
    val input = readInput("input.txt")

    val part1 = solve1(input)
    val part2 = solve2(input)

    println("Part1: $part1")
    println("Part2: $part2")
}
