using System;
using System.Diagnostics;
using System.Linq;
using System.IO;
using System.Text.RegularExpressions;

using Values = System.Collections.Generic.IEnumerable<string>;
using Passports = System.Collections.Generic.IEnumerable<System.Collections.Generic.IEnumerable<string>>;

bool ValidFormat(Values values) =>
    new string[]{"byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"}
        .All(start => values.Any(x => x.StartsWith(start)));

bool ValidValue(string value) =>
    new Regex(value.Substring(0,3) switch {
            "byr" => "^byr:(19[2-9][0-9]|200[0-2])$",
            "iyr" => "^iyr:(201[0-9]|2020)$",
            "eyr" => "^eyr:(202[0-9]|2030)$",
            "hgt" => "^hgt:(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)$",
            "hcl" => "^hcl:#[a-f0-9]{6}$",
            "ecl" => "^ecl:(amb|blu|brn|gry|grn|hzl|oth)$",
            "pid" => "^pid:[0-9]{9}$",
            "cid" => "^cid:",
            _     => "$^",
        }).IsMatch(value);

bool ValidValues(Values values) =>
    values.All(ValidValue);

Values ParseValues(string input) =>
    input.Split(" ");

int Solve1(Passports passports) =>
    passports.Where(ValidFormat).Count();

int Solve2(Passports passports) =>
    passports.Where(ValidFormat)
        .Where(ValidValues)
        .Count();


Passports input = File.ReadAllText("input.txt")
    .Split("\r\n\r\n")
    .Select(x => x.Replace(System.Environment.NewLine, " "))
    .Select(ParseValues);

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input);
var part2 = Solve2(input);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");
