import java.io.File

data class Instruction(val instruction: String, val value: Long)

data class Position(val horizontal: Long, val depth: Long, val aim: Long) {
    val part1 = this.horizontal * this.aim
    val part2 = this.horizontal * this.depth
}

fun execute(instruction: Instruction, pos: Position): Position = 
    when(instruction.instruction) {
        "forward" -> Position(pos.horizontal + instruction.value, pos.depth + pos.aim * instruction.value, pos.aim)
        "down"    -> Position(pos.horizontal, pos.depth, pos.aim + instruction.value)
        else      -> Position(pos.horizontal, pos.depth, pos.aim - instruction.value)
    }

fun solve(input: List<Instruction>): Position =
    input.fold(Position(0, 0, 0)) { pos, instruction -> execute(instruction, pos) }


val input = File("input.txt")
    .readLines()
    .map { it.split(' ') }
    .map { (instruction, value) -> Instruction(instruction, value.toLong()) }

val solution = solve(input)

println("Part1: ${solution.part1}")
println("Part2: ${solution.part2}")
