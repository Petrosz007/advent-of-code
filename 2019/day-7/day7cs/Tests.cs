using System.Linq;
using Xunit;
using System;
using day7cs;

namespace day7cs.Tests
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

        [Theory]
        [InlineData(new int[]{3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0}, 43210)]
        [InlineData(new int[]{3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                101,5,23,23,1,24,23,23,4,23,99,0,0}, 54321)]
        [InlineData(new int[]{3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0}, 65210)]
        public void TestThrusters(int[] code, int solution)
        {
            var result = ThusterLogic.SolvePart1(code);
            Assert.Equal(result, solution);
        }

        [Theory]
        [InlineData(new int[]{3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5}, 139629729)]
        [InlineData(new int[]{3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10}, 18216)]
        public void TestThrusters2(int[] code, int solution)
        {
            var result = ThusterLogic.SolvePart2(code);
            Assert.Equal(result, solution);
        }
    }
}