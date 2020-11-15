using System.Linq;

namespace day7cs
{
    public static class ThusterLogic
    {
        private static bool NextPermutation(int[] array) {
            // Find non-increasing suffix
            int i = array.Length - 1;
            while (i > 0 && array[i - 1] >= array[i])
                i--;
            if (i <= 0)
                return false;
            
            // Find successor to pivot
            int j = array.Length - 1;
            while (array[j] <= array[i - 1])
                j--;
            int temp = array[i - 1];
            array[i - 1] = array[j];
            array[j] = temp;
            
            // Reverse suffix
            j = array.Length - 1;
            while (i < j) {
                temp = array[i];
                array[i] = array[j];
                array[j] = temp;
                i++;
                j--;
            }
            return true;
        }
        
        public static int SolvePart1(int[] code)
        {
            var phaseSettings = new int[]{0,1,2,3,4};
            int maxOutput = 0;
            do {
                int prevOutput = 0;
                for(int i = 0; i < 5; ++i)
                {
                    int[] localCode = (int[])code.Clone();
                    var cpu = new IntCodeCPU(localCode, new int[]{phaseSettings[i], prevOutput});
                    cpu.Run();
                    
                    prevOutput = cpu.Outputs.Last();
                }

                if(maxOutput < prevOutput)
                    maxOutput = prevOutput;
            } while(NextPermutation(phaseSettings));

            return maxOutput;
        }

        public static int SolvePart2(int[] code)
        {
            var phaseSettings = new int[]{5,6,7,8,9};

            int maxOutput = 0;
            do {
                var cpus = new IntCodeCPU[5];
                int prevOutput = 0;
                while(!(cpus[4]?.Halted ?? false))
                {
                    for(int i = 0; i < 5; ++i)
                    {
                        cpus[i] ??= new ((int[])code.Clone(), new int[]{phaseSettings[i]});
                        
                        cpus[i].Inputs.Enqueue(prevOutput);
                        cpus[i].RunUntilWaitingInput();
                        
                        prevOutput = cpus[i].Outputs.Last();
                    }
                }

                if(maxOutput < prevOutput)
                    maxOutput = prevOutput;
            } while(NextPermutation(phaseSettings));

            return maxOutput;
        }
    }
}