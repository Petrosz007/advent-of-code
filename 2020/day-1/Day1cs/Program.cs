using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

namespace Day1cs
{
    public class Program
    {
        public static long Solve1(List<long> nums)
        {
            for(int i = 0; i < nums.Count; ++i)
                for(int j = i+1; j < nums.Count; ++j)
                    if(nums[i]+nums[j] == 2020)
                        return nums[i]*nums[j];

            return -1;
        }

        public static long Solve2(List<long> nums)
        {
            for(int i = 0; i < nums.Count; ++i)
                for(int j = i+1; j < nums.Count; ++j)
                    for(int k = j+1; k < nums.Count; ++k)
                        if(nums[i]+nums[j]+nums[k] == 2020)
                            return nums[i]*nums[j]*nums[k];

            return -1;
        }
        
        public static void Main(string[] args)
        {
            var nums = File.ReadAllLines("input.txt").Select(long.Parse).ToList();

            var stopwatch = Stopwatch.StartNew();

            var part1 = Solve1(nums);
            var part2 = Solve2(nums);

            stopwatch.Stop();

            Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
            Console.WriteLine($"Part1: {part1}");
            Console.WriteLine($"Part2: {part2}");
        }
    }
}
