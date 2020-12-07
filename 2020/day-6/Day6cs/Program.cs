using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

int CountChars(string[] group, Func<IEnumerable<char>, IEnumerable<char>, IEnumerable<char>> aggregator) =>
    group
        .Select(Enumerable.ToList)
        .Aggregate(aggregator)
        .Count();

int Solve1(string[][] input) =>
    input
        .Select(group => CountChars(group, Enumerable.Union))
        .Sum();
        
int Solve2(string[][] input) =>
    input
        .Select(group => CountChars(group, Enumerable.Intersect))
        .Sum();


var input = File.ReadAllText("input.txt")
    .Split(Environment.NewLine+Environment.NewLine)
    .Select(x => x.Split(Environment.NewLine))
    .ToArray();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input);
var part2 = Solve2(input);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");
