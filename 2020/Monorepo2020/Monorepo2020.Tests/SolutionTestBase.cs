
namespace Monorepo2020.Tests;

public abstract class SolutionTestBase<TSolution, T> 
    where TSolution : SolutionBase<T>, new()
{
    protected readonly TSolution Solution = new();
    protected abstract long? Part1Solution { get; }
    protected abstract long? Part2Solution { get; }
    
    protected string[] ReadTestInput(string fileName) =>
        File.ReadAllLines(Path.Combine("TestInputs", Solution.DayName, fileName));

    [Fact]
    protected void Part1FinalAnswer()
    {
        if (Part1Solution is null) return;
        
        var result = Solution.GetPart1();
        Assert.Equal(Part1Solution, result);
    }
    
    [Fact]
    protected void Part2FinalAnswer()
    {
        if (Part2Solution is null) return;
        
        var result = Solution.GetPart2();
        Assert.Equal(Part2Solution, result);
    }
    
    private void TestSolveForInput(string fileName, long expected, bool solve1)
    {
        // Arrange
        var testInput = ReadTestInput(fileName);
        var input = Solution.ParseInput(testInput);
        
        // Act
        var result = solve1
            ? Solution.Solve1(input)
            : Solution.Solve2(input);
        
        // Assert
        Assert.Equal(expected, result);
    }

    protected void TestSolve1ForInput(string fileName, long expected) =>
        TestSolveForInput(fileName, expected, true);
    
    protected void TestSolve2ForInput(string fileName, long expected) =>
        TestSolveForInput(fileName, expected, false);
}