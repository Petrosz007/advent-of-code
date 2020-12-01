using System.Collections.Generic;
using System.Linq;
using Xunit;
using Day1cs;

namespace Day1cs.Tests
{
    public class Tests
    {
        [Fact]
        public void TestSolve2()
        {
            var nums = new List<long>{ 1721, 979, 366, 299, 675, 1456 }.OrderBy(x => x).ToList();
            var result = Program.Solve2(nums);
            Assert.Equal(result, 241861950);
        }
    }
}