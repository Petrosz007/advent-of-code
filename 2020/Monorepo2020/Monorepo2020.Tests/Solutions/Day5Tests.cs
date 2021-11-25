namespace Monorepo2020.Tests.Solutions;

public class Day5Tests : SolutionTestBase<Day5, string[]>
{
    protected override long? Part1Solution => 994;
    protected override long? Part2Solution => 741;
    
    [Theory]
    [InlineData("FBFBBFFRLR", 357)]
    [InlineData("BFFFBBFRRR", 567)]
    [InlineData("FFFBBBFRRR", 119)]
    [InlineData("BBFFBBFRLL", 820)]
    public void TestSeatID(string input, int expected)
    {
        var result = Solution.SeatID(input);

        Assert.Equal(expected, result);
    }
}