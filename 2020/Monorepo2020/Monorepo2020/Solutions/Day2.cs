using System.Text.RegularExpressions;

namespace Monorepo2020.Solutions;

public class Day2 : SolutionBase<List<Day2.Password>>
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
            var count = password.Count(x => x == c);
            return count >= min && count <= max;
        }

        public bool IsValidPart2() =>
            password[min-1] == c ^ password[max-1] == c;
    }

    public override List<Password> ParseInput(IEnumerable<string> input) =>
        input.Select(Password.Parse).ToList();

    public override long Solve1(List<Password> passwords) =>
        passwords.Count(p => p.IsValidPart1());

    public override long Solve2(List<Password> passwords) =>
        passwords.Count(p => p.IsValidPart2());
}