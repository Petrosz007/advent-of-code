using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

namespace DayXcs
{
    public class Program
    {   
        public static void Main(string[] args)
        {
            var input = File.ReadAllLines("input.txt");

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
