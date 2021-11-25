namespace Monorepo2020.Tests.Solutions;

public class Day10Tests : SolutionTestBase<Day10, int[]>
{
    protected override long? Part1Solution => 1914;
    protected override long? Part2Solution => 9256148959232;
    
    [Fact]
    void Solve1() => TestSolve1ForInput("test1.txt", 35);
    
    [Fact]
    void Solve2() => TestSolve2ForInput("test2.txt", 19208);
}