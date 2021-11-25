namespace Monorepo2020.Tests.Solutions;

public class Day1Tests : SolutionTestBase<Day1, List<long>>
{
    protected override long? Part1Solution => 100419;
    protected override long? Part2Solution => 265253940;

    [Fact]
    void TestPart1()
    {
        var nums = new List<long> { 1721, 979, 366, 299, 675, 1456 };

        var result = Solution.Solve1(nums);
        
        Assert.Equal(514579, result);
    }
    
    [Fact]
    void TestPart2()
    {
        var nums = new List<long> { 1721, 979, 366, 299, 675, 1456 };

        var result = Solution.Solve2(nums);
        
        Assert.Equal(241861950, result);
    }
}