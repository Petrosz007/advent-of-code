using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;
using System.Text.RegularExpressions;

namespace Day4cs
{
    public class Program
    {
        public static bool ValidFormat(string inp)
        {
            var test = new string[]{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"};
            var a = test.Where(x => inp.Contains(x+":")).Count();
            return a == test.Length;
        }

        public static int Solve1(List<string> input) =>
            input.Where(ValidFormat).Count();

        public static bool IntBetween(string input, int low, int high)
        {
            int val;
            if(int.TryParse(input, out val))
                return (val >= low) && (val <= high);
            return false;
        }

        public static bool ValidValues(string input)
        {
            // Console.WriteLine($"{input}\n");
            var kvs = input.Split(" ").Select(x => (x.Split(":")[0], x.Split(":")[1]));

            bool isValid = true;
            foreach((string k, string v) in kvs)
            {
                switch(k)
                {
                    case "byr":
                        var ret = IntBetween(v, 1920, 2002);
                        // Console.WriteLine($"{k}:{v} is {ret}");
                        isValid = isValid && ret;
                    break;
                    
                    case "iyr":
                        var ret1 = IntBetween(v, 2010, 2020);
                        // Console.WriteLine($"{k}:{v} is {ret1}");
                        isValid = isValid && ret1;
                    break;

                    case "eyr":
                        var ret2 = IntBetween(v, 2020, 2030);
                        // Console.WriteLine($"{k}:{v} is {ret2}");
                        isValid = isValid && ret2;
                    break;

                    case "hgt":
                        bool ret3 = false;
                        if(v.Substring(v.Length-2).Equals("in"))
                            ret3 = IntBetween(v.Substring(0, v.Length-2), 59, 76);
                        else if(v.Substring(v.Length-2).Equals("cm"))
                            ret3 = IntBetween(v.Substring(0, v.Length-2), 150, 193);
                        else 
                            ret3 = false;
                        // Console.WriteLine($"{k}:{v} is {ret3}");
                        isValid = isValid && ret3;
                    break;

                    case "hcl":
                        var ret4 = new Regex(@"#[a-f0-9]{6}").IsMatch(v);
                        // Console.WriteLine($"{k}:{v} is {ret4}");
                        isValid = isValid && ret4;
                    break;

                    case "ecl":
                        var ret5 = new Regex(@"(amb|blu|brn|gry|grn|hzl|oth)").IsMatch(v);
                        // Console.WriteLine($"{k}:{v} is {ret5}");
                        isValid = isValid && ret5;
                    break;

                    case "pid":
                        var ret6 = new Regex(@"^[0-9]{9}$").IsMatch(v);
                        // Console.WriteLine($"{k}:{v} is {ret6}");
                        isValid = isValid && ret6;
                    break;

                    case "cid":
                    break;

                    default:
                        // Console.WriteLine($"{k}:{v} is DEFAULTED");
                        isValid = isValid && false;
                    break;
                }
            }
            // Console.WriteLine($"OVERALL: {isValid}");
            return isValid;
        }

        public static int Solve2(List<string> input) =>
            input.Where(ValidFormat).Where(ValidValues).Count();

        public static void Main(string[] args)
        {
            var input = File.ReadAllText("input.txt").Split("\r\n\r\n").Select(x => x.Replace(System.Environment.NewLine, " ")).ToList();

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
