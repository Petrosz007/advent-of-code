using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

long Solve1(List<int> numbers) =>
    numbers.Zip(numbers.Skip(1))
        .Count(x => x.First < x.Second);

long Solve2(List<int> numbers) =>
    Solve1(numbers
        .Zip(numbers.Skip(1))
        .Zip(numbers.Skip(2), (x, y) => x.First + x.Second + y)
        .ToList());

var input = File.ReadAllLines("input.txt")
    .Select(int.Parse)
    .ToList();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input);
var part2 = Solve2(input);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");
