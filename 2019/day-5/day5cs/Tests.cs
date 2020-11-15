using System.Linq;
using Xunit;
using System;
using day5cs;

namespace day5cs.Tests
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
            var cpu = new IntCodeCPU(input, new int[]{});
            cpu.Run();
            Assert.True(cpu.Memory.SequenceEqual(output));
        }

        [Fact]
        public void TestIO()
        {
            var cpu = new IntCodeCPU(new int[]{3,0,4,0,99}, new int[]{33});
            cpu.Run();
            Assert.True(cpu.Outputs.SequenceEqual(new int[]{33}));
        }

        [Theory]
        [InlineData(new int[]{3,9,8,9,10,9,4,9,99,-1,8}, new int[]{8}, new int[]{1})] // equals
        [InlineData(new int[]{3,9,8,9,10,9,4,9,99,-1,8}, new int[]{7}, new int[]{0})] // equals
        [InlineData(new int[]{3,9,7,9,10,9,4,9,99,-1,8}, new int[]{7}, new int[]{1})] // less than
        [InlineData(new int[]{3,9,7,9,10,9,4,9,99,-1,8}, new int[]{8}, new int[]{0})] // less than

        [InlineData(new int[]{3,3,1108,-1,8,3,4,3,99}, new int[]{8}, new int[]{1})] // equals
        [InlineData(new int[]{3,3,1108,-1,8,3,4,3,99}, new int[]{7}, new int[]{0})] // equals
        [InlineData(new int[]{3,3,1107,-1,8,3,4,3,99}, new int[]{7}, new int[]{1})] // less than
        [InlineData(new int[]{3,3,1107,-1,8,3,4,3,99}, new int[]{8}, new int[]{0})] // less than

        [InlineData(new int[]{3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9}, new int[]{0}, new int[]{0})] // input is 0 or non0
        [InlineData(new int[]{3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9}, new int[]{100}, new int[]{1})] // input is 0 or non0
        [InlineData(new int[]{3,3,1105,-1,9,1101,0,0,12,4,12,99,1}, new int[]{0}, new int[]{0})] // input is 0 or non0
        [InlineData(new int[]{3,3,1105,-1,9,1101,0,0,12,4,12,99,1}, new int[]{100}, new int[]{1})] // input is 0 or non0

        [InlineData(new int[]{3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99}, 
                    new int[]{7}, new int[]{999})] // <8: 999, =8: 1000 >8: 1001
        [InlineData(new int[]{3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99}, 
                    new int[]{8}, new int[]{1000})] // <8: 999, =8: 1000 >8: 1001
        [InlineData(new int[]{3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99}, 
                    new int[]{9}, new int[]{1001})] // <8: 999, =8: 1000 >8: 1001
        public void TestJumps(int[] code, int[] inputs, int[] outputs)
        {
            var cpu = new IntCodeCPU(code, inputs);
            cpu.Run();
            Assert.True(cpu.Outputs.SequenceEqual(outputs));
        }
    }
}