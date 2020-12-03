using System.Collections.Generic;
using System.Linq;
using Xunit;
using Day3cs;
using System.IO;

namespace Day3cs.Tests
{
    public class Tests
    {
        [Fact]
        public void TestSolve1()
        {
            var input = File.ReadAllLines("test1.txt");
            var result = Program.Solve1(input);
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
            var input = File.ReadAllLines("test1.txt");
            var result = Program.CheckSlope(input, rightAmount, downAmount);
            Assert.Equal(expectedResult, result);
        }

        [Fact]
        public void TestSolve2()
        {
            var input = File.ReadAllLines("test1.txt");
            var result = Program.Solve2(input);
            Assert.Equal(336, result);
        }
    }
}