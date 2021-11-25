namespace Monorepo2020.Tests.Solutions;

public class Day8Tests : SolutionTestBase<Day8, (string,int)[]>
{
    protected override long? Part1Solution => 1317;
    protected override long? Part2Solution => 1033;
    
    [Fact]
    void Solve1() => TestSolve1ForInput("test1.txt", 5);
    
    [Fact]
    void Solve2() => TestSolve2ForInput("test2.txt", 8);
}