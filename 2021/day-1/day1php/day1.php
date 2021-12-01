<?php

function readInput(string $fileName): array {
    $text = file_get_contents($fileName);
    $lines = explode("\n", $text);
    $numbers = array_map(fn($x) => intval($x), $lines);
    return $numbers;
}

function solve1(array $numbers): int {
    $reduced = array_reduce($numbers, fn($prev, $x) =>
        [$x, $prev[1] + ($prev[0] < $x ? 1 : 0)]
    , [$numbers[0], 0]);

    return $reduced[1];
}

function solve2(array $numbers): int {
    $reduced = array_reduce($numbers, function($acc, $current) {
        [$prev2, $prev1, $previousResult, $sum] = $acc;
        $currentResult = $prev2 + $prev1 + $current;
        $newSum = $sum + ($previousResult < $currentResult ? 1 : 0);

        return [$prev1, $current, $currentResult, $newSum];
    }, [$numbers[1], $numbers[2], $numbers[0] + $numbers[1] + $numbers[2], 0]);

    return $reduced[3];
}

$numbers = readInput("input.txt");
$part1 = solve1($numbers);
$part2 = solve2($numbers);

print("Part 1: " . $part1 . "\n");
print("Part 2: " . $part2 . "\n");

?>