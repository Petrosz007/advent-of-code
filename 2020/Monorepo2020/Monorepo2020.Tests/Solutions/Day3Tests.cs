namespace Monorepo2020.Tests.Solutions;

public class Day3Tests : SolutionTestBase<Day3, string[]>
{
    protected override long? Part1Solution => 292;
    protected override long? Part2Solution => 9354744432;

    private readonly string[] _testInput1;
    
    public Day3Tests()
    {
        _testInput1 = ReadTestInput("test1.txt");
    }
    
    [Fact]
    public void TestSolve1()
    {
        var result = Solution.Solve1(_testInput1);
        Assert.Equal(7, result);
    }

    [Theory]
    [InlineData(1,1,2)]
    [InlineData(3,1,7)]
    [InlineData(5,1,3)]
    [InlineData(7,1,4)]
    [InlineData(1,2,2)]
    public void TestCheckSlope(int rightAmount, int downAmount, int expectedResult)
    {
        var result = Day3.CheckSlope(_testInput1, rightAmount, downAmount);
        Assert.Equal(expectedResult, result);
    }
}