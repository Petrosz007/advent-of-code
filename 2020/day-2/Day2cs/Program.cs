using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;
using System.Text.RegularExpressions;

namespace Day2cs
{
    public class Program
    {   
        public record Password(int min, int max, char c, string password)
        {
            public static Password Parse(string input)
            {
                var pattern = @"(?<min>\d+)-(?<max>\d+) (?<c>.): (?<password>.*)";
                Regex rg = new(pattern);
                var group = rg.Match(input).Groups;
                return new (
                        int.Parse(group["min"].Value),
                        int.Parse(group["max"].Value), 
                        char.Parse(group["c"].Value),
                        group["password"].Value);
            }
            public bool IsValidPart1()
            {
                var count = password.Where(x => x == c).Count();
                return count >= min && count <= max;
            }

            public bool IsValidPart2() =>
                password[min-1] == c ^ password[max-1] == c;
        }

        public static int Solve1(List<Password> passwords) =>
            passwords.Where(p => p.IsValidPart1()).Count();

        public static int Solve2(List<Password> passwords) =>
            passwords.Where(p => p.IsValidPart2()).Count();

        public static void Main(string[] args)
        {

            var passwords = File.ReadAllLines("input.txt").Select(Password.Parse).ToList();

            var stopwatch = Stopwatch.StartNew();

            var part1 = Solve1(passwords);
            var part2 = Solve2(passwords);

            stopwatch.Stop();

            Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
            Console.WriteLine($"Part1: {part1}");
            Console.WriteLine($"Part2: {part2}");
        }
    }
}
