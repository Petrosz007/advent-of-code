namespace Monorepo2020.Tests.Solutions;

public class Day9Tests : SolutionTestBase<Day9, List<long>>
{
    protected override long? Part1Solution => 14144619;
    protected override long? Part2Solution => 1766397;

    [Fact]
    public void Solve1()
    {
        var input = ReadTestInput("test1.txt");
        var parsedInput = Solution.ParseInput(input);
        
        var result = Solution.Solve1(parsedInput, 5);
        
        Assert.Equal(127, result);
    }
    
    [Fact]
    public void Solve2()
    {
        var input = ReadTestInput("test2.txt");
        var parsedInput = Solution.ParseInput(input);
        
        var result = Solution.Solve2(parsedInput, 5);
        
        Assert.Equal(62, result);
    }
}