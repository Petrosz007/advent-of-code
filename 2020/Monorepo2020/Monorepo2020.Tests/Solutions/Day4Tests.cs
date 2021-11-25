using System.IO;

namespace Monorepo2020.Tests.Solutions;

public class Day4Tests : SolutionTestBase<Day4, IEnumerable<IEnumerable<string>>>
{
    protected override long? Part1Solution => 222;
    protected override long? Part2Solution => 140;

    [Fact]
    public void Solve1() => TestSolve1ForInput("test1.txt", 2);

    [Fact]
    public void Solve2Invalids() => TestSolve2ForInput("test2.txt", 0);

    [Fact]
    public void Solve2Valids() => TestSolve2ForInput("test3.txt", 4);
}