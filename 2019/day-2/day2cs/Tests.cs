using System.Linq;
using Xunit;
using System;
using day2cs;

namespace day2cs.Tests
{
    public class IntCodeCPUTests
    {
        [Theory]
        [InlineData(new int[]{1,0,0,0,99}, new int[]{2,0,0,0,99})]
        [InlineData(new int[]{2,3,0,3,99}, new int[]{2,3,0,6,99})]
        [InlineData(new int[]{2,4,4,5,99,0}, new int[]{2,4,4,5,99,9801})]
        [InlineData(new int[]{1,1,1,4,99,5,6,0,99}, new int[]{30,1,1,4,2,5,6,0,99})]
        public void Test(int[] input, int[] output)
        {
            var cpu = new IntCodeCPU(input);
            cpu.Run();
            Assert.True(cpu.Memory.SequenceEqual(output));
        }

    }
}