using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

namespace Day5cs
{
    public class Program
    {   
        public static int BinaryLikeSearch(string input, int low, int high, char lowDelimiter)
        {
            foreach(char c in input.Substring(0, input.Length - 1))
            {
                if(c == lowDelimiter) 
                    high = (low + high) / 2;
                else 
                    low = (low + high) / 2 + 1;
            }
            int value = input.Last() == lowDelimiter ? low : high;
            return value;
        }

        public static int SeatID(string input)
        {
            int row = BinaryLikeSearch(input.Substring(0, 7), 0, 127, 'F');
            int col = BinaryLikeSearch(input.Substring(7, 3), 0,   7, 'L');

            return row * 8 + col;
        }

        public static int Solve1(string[] input) =>
            input.Select(SeatID).Max();

        public static int Solve2(string[] input)
        {
            var ids = input.Select(SeatID).OrderBy(x => x).ToList();
            for(int i = 0; i < ids.Count - 1; ++i)
                if(ids[i] + 1 != ids[i + 1])
                    return ids[i] + 1;

            throw new Exception("Seat not found");
        }

        public static void Main(string[] args)
        {
            var asd = SeatID("FBFBBFFRLR");
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
