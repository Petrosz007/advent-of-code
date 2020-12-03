using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

namespace Day3cs
{
    public class Program
    {   
        public static int CheckSlope(string[] input, int rightAmount, int downAmount)
        {
            (int row, int col, int treeCount) = (downAmount, (int) rightAmount, 0);
            while(row < input.Length)
            {
                if(input[row][col] == '#') ++treeCount;

                col = (col + rightAmount) % input[row].Length;
                row += downAmount;
            }

            return treeCount;
        }

        public static int Solve1(string[] input) => 
            CheckSlope(input, 3, 1);

        public static long Solve2(string[] input) =>
            new (int,int)[]{ (1,1), (3,1), (5,1), (7,1), (1,2) }
                .Select(x => (long) CheckSlope(input, x.Item1, x.Item2))
                .Aggregate((acc, x) => acc * x);

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
