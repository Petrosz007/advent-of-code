namespace Monorepo2020.Solutions;

public class Day6 : SolutionBase<string[][]>
{
    public override string[][] ParseInput(IEnumerable<string> input) =>
        string.Join(Environment.NewLine, input)
            .Split(Environment.NewLine + Environment.NewLine)
            .Select(x => x.Split(Environment.NewLine))
            .ToArray();

    int CountChars(string[] group, Func<IEnumerable<char>, IEnumerable<char>, IEnumerable<char>> aggregator) =>
        group
            .Select(Enumerable.ToList)
            .Aggregate(aggregator)
            .Count();

    public override long Solve1(string[][] input) =>
        input
            .Select(group => CountChars(group, Enumerable.Union))
            .Sum();

    public override long Solve2(string[][] input) =>
        input
            .Select(group => CountChars(group, Enumerable.Intersect))
            .Sum();
}