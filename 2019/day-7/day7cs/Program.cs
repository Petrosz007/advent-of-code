using System.Threading;
using System.Diagnostics;
using System.Linq;
using System.IO;
using System;
using day7cs;

var code = File.ReadAllText("input.txt").Split(',').Select(x => int.Parse(x)).ToArray();

var stopwatch = Stopwatch.StartNew();

var part1 = ThusterLogic.SolvePart1(code);
var part2 = ThusterLogic.SolvePart2(code);

stopwatch.Stop();

Console.WriteLine($"Solved in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part 1: {part1}");
Console.WriteLine($"Part 2: {part2}");
