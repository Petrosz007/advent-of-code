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
            (int i, int j) = (0, nums.Count - 1);

            while(true) {
                var result = nums[i] + nums[j];
                if(result == 2020)
                    return nums[i] * nums[j];
                else if(result < 2020)
                    ++i;
                else if(result >= 2020)
                    --j;
            }
        }

        public static long Solve1Alt(List<long> nums)
        {
            for(int i = 0; i < nums.Count; ++i)
                for(int j = i+1; j < nums.Count; ++j)
                    if(nums[i]+nums[j] == 2020)
                        return nums[i]*nums[j];

            return -1;
        }


        public static long Solve2(List<long> nums)
        {
            for(int k = 0; k < nums.Count; ++k)
            {
                (int i, int j) = (k+1, nums.Count - 1);

                while(true) {
                    if(i == j) break;
                    if(k == i || k == j) continue;

                    var result = nums[i] + nums[j] + nums[k];
                    if(result == 2020)
                        return nums[i] * nums[j] * nums[k];
                    else if(result < 2020)
                        ++i;
                    else if(result >= 2020)
                        --j;
                }
            }

            return 0;
        }

        public static long Solve2Alt(List<long> nums)
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
            var nums = File.ReadAllLines("input.txt").Select(long.Parse).OrderBy(x => x).ToList();

            var stopwatch = Stopwatch.StartNew();

            var part1 = Solve1(nums);
            var part2 = Solve2(nums);

            stopwatch.Stop();

            Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
            Console.WriteLine($"Part1: {part1}");
            Console.WriteLine($"Part2: {part2}");

            
            stopwatch = Stopwatch.StartNew();

            var part1alt = Solve1Alt(nums);
            var part2alt = Solve2Alt(nums);

            stopwatch.Stop();

            Console.WriteLine($"Alternative done in {stopwatch.Elapsed.TotalMilliseconds}ms");
            Console.WriteLine($"Part1: {part1alt}");
            Console.WriteLine($"Part2: {part2alt}");
        }
    }
}
