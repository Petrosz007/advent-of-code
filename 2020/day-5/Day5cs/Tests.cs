using System.Collections.Generic;
using System.Linq;
using Xunit;
using Day5cs;

namespace Day5cs.Tests
{
    public class Tests
    {
        [Theory]
        [InlineData("FBFBBFFRLR", 357)]
        [InlineData("BFFFBBFRRR", 567)]
        [InlineData("FFFBBBFRRR", 119)]
        [InlineData("BBFFBBFRLL", 820)]
        public void TestSeatID(string input, int expected)
        {
            var result = Program.SeatID(input);

            Assert.Equal(expected, result);
        }
    }
}