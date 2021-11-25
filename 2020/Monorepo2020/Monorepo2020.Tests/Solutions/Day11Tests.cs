namespace Monorepo2020.Tests.Solutions;

public class Day11Tests : SolutionTestBase<Day11, char[,]>
{
    protected override long? Part1Solution => 2470;
    protected override long? Part2Solution => 2259;
    
    [Fact]
    void Solve1() => TestSolve1ForInput("test1.txt", 37);
    
    [Fact]
    void Solve2() => TestSolve2ForInput("test2.txt", 26);
}