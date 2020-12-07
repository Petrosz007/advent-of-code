using System.Collections.Generic;
using System.Linq;
using Xunit;
using Day7cs;

namespace Day7cs.Tests
{
    public class Tests
    {
        [Theory]
        [InlineData("light red bags contain 1 bright white bag, 2 muted yellow bags.", 
            "light red", new string[]{ "bright white", "muted yellow" }, new int[]{ 1, 2 })]
        [InlineData("dark orange bags contain 3 bright white bags, 4 muted yellow bags.", 
            "dark orange", new string[]{ "bright white", "muted yellow" }, new int[]{ 3, 4 })]
        [InlineData("bright white bags contain 1 shiny gold bag.", 
            "bright white", new string[]{ "shiny gold" }, new int[]{ 1 })]
        [InlineData("faded blue bags contain no other bags.", 
            "faded blue", new string[]{}, new int[]{})]
        public void TestParseLine(string line, string container, string[] contained, int[] containedCount)
        {
            var result = Program.ParseLine(line);

            Assert.Equal(container, result.Item1);
            Assert.True(contained.SequenceEqual(result.Item2.Select(x => x.Item1)));
            Assert.True(containedCount.SequenceEqual(result.Item2.Select(x => x.Item2)));
        }
    }
}