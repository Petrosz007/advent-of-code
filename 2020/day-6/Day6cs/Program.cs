﻿using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

namespace Day6cs
{
    public class Program
    {   
        public static int CountChars(string[] group, Func<IEnumerable<char>, IEnumerable<char>, IEnumerable<char>> aggregator) =>
            group
                .Select(Enumerable.ToList)
                .Aggregate(aggregator)
                .Count();

        public static int Solve1(string[][] input) =>
            input
                .Select(group => CountChars(group, Enumerable.Union))
                .Sum();
        public static int Solve2(string[][] input) =>
            input
                .Select(group => CountChars(group, Enumerable.Intersect))
                .Sum();

        public static void Main(string[] args)
        {
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
        }
    }
}
