namespace Monorepo2020.Tests.Solutions;

public class Day6Tests : SolutionTestBase<Day6, string[][]>
{
    protected override long? Part1Solution => 6596;
    protected override long? Part2Solution => 3219;
    

    [Fact]
    void Solve1() => TestSolve1ForInput("test1.txt", 11);
    
    [Fact]
    void Solve2() => TestSolve2ForInput("test1.txt", 6);
}