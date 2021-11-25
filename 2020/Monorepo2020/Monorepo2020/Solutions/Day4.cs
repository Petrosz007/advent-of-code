using System.Collections;
using System.Text.RegularExpressions;

namespace Monorepo2020.Solutions;

using Values = IEnumerable<string>;
using Passports = IEnumerable<IEnumerable<string>>;

public class Day4 : SolutionBase<Passports>
{
    bool ValidFormat(Values values) =>
        new [] { "byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:" }
            .All(start => values.Any(x => x.StartsWith(start)));

    bool ValidValue(string value) =>
        new Regex(value[..3] switch
        {
            "byr" => "^byr:(19[2-9][0-9]|200[0-2])$",
            "iyr" => "^iyr:(201[0-9]|2020)$",
            "eyr" => "^eyr:(202[0-9]|2030)$",
            "hgt" => "^hgt:(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)$",
            "hcl" => "^hcl:#[a-f0-9]{6}$",
            "ecl" => "^ecl:(amb|blu|brn|gry|grn|hzl|oth)$",
            "pid" => "^pid:[0-9]{9}$",
            "cid" => "^cid:",
            _ => "$^",
        }).IsMatch(value);

    bool ValidValues(Values values) =>
        values.All(ValidValue);

    Values ParseValues(string input) =>
        input.Split(" ");


    public override Passports ParseInput(IEnumerable<string> input) =>
        string.Join(Environment.NewLine, input)
            .Split(Environment.NewLine + Environment.NewLine)
            .Select(x => x.Replace(Environment.NewLine, " "))
            .Select(ParseValues);

    public override long Solve1(Passports passports) =>
        passports.Where(ValidFormat).Count();

    public override long Solve2(Passports passports) =>
        passports.Where(ValidFormat)
            .Where(ValidValues)
            .Count();
}