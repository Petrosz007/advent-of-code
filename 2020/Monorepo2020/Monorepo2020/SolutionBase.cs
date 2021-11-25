using System.Diagnostics;

namespace Monorepo2020;

public abstract class SolutionBase<T>
{
    public abstract T ParseInput(IEnumerable<string> input);
    public abstract long Solve1(T input);
    public abstract long Solve2(T input);

    public string DayName => GetType().Name;

    public void RunSolutions()
    {
        var lines = File.ReadAllLines(Path.Combine("Inputs", $"{DayName}.txt"));
        var input = ParseInput(lines);

        var stopwatch = Stopwatch.StartNew();

        var part1 = Solve1(input);
        var part2 = Solve2(input);

        stopwatch.Stop();

        Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
        Console.WriteLine($"Part1: {part1}");
        Console.WriteLine($"Part2: {part2}");
    }
    
    public long GetPart1()
    {
        var lines = File.ReadAllLines(Path.Combine("Inputs", $"{DayName}.txt"));
        var input = ParseInput(lines);

        var part1 = Solve1(input);

        return part1;
    }
    
    public long GetPart2()
    {
        var lines = File.ReadAllLines(Path.Combine("Inputs", $"{DayName}.txt"));
        var input = ParseInput(lines);

        var part2 = Solve2(input);

        return part2;
    }
}
