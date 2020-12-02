using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;
using Day2cs;

namespace Day2cs.Tests
{
    public class Tests
    {
        [Fact]
        public void TestSolve1()
        {
            var input = @"1-3 a: abcde
                1-3 b: cdefg
                2-9 c: ccccccccc";
            var passwords = input.Split(Environment.NewLine).Select(Program.Password.Parse).ToList();
            var result = Program.Solve1(passwords);
            Assert.Equal(2, result);
        }

        [Fact]
        public void TestSolve2()
        {
            var input = @"1-3 a: abcde
                1-3 b: cdefg
                2-9 c: ccccccccc";
            var passwords = input.Split(Environment.NewLine).Select(Program.Password.Parse).ToList();
            var result = Program.Solve2(passwords);
            Assert.Equal(1, result);
        }
    }
}