using System.Linq;

namespace Monorepo2020.Tests.Solutions;

public class Day7Tests : SolutionTestBase<Day7, ILookup<string, (string Name, int Count)>>
{
    protected override long? Part1Solution => 229;
    protected override long? Part2Solution => 6683;
    
    [Theory]
    [InlineData("light red bags contain 1 bright white bag, 2 muted yellow bags.", 
        "light red", new []{ "bright white", "muted yellow" }, new []{ 1, 2 })]
    [InlineData("dark orange bags contain 3 bright white bags, 4 muted yellow bags.", 
        "dark orange", new []{ "bright white", "muted yellow" }, new []{ 3, 4 })]
    [InlineData("bright white bags contain 1 shiny gold bag.", 
        "bright white", new []{ "shiny gold" }, new []{ 1 })]
    [InlineData("faded blue bags contain no other bags.", 
        "faded blue", new string[]{}, new int[]{})]
    public void TestParseLine(string line, string container, string[] contained, int[] containedCount)
    {
        var result = Solution.ParseLine(line);

        Assert.True(Enumerable.Repeat(container, result.Count).SequenceEqual(result.Select(x => x.Parent)));
        Assert.True(contained.SequenceEqual(result.Select(x => x.Child)));
        Assert.True(containedCount.SequenceEqual(result.Select(x => x.Count)));
    }
    
    [Fact]
    void Solve1() => TestSolve1ForInput("test1.txt", 4);
    
    [Fact]
    void Solve2() => TestSolve2ForInput("test2.txt", 126);
}