using System;
using System.Collections.Generic;

namespace day7cs
{
    public enum Mode : int
    {
        Position = 0,
        Immediate = 1,
    }

    public class IntCodeCPU
    {
        public int[] Memory { get; private set; }
        private int ip;
        public bool Halted { get; private set; }
        public Queue<int> Inputs { get; private set; }
        public Queue<int> Outputs { get; private set; }

        public IntCodeCPU(int[] code, IEnumerable<int> inputs)
        {
            Memory = code;
            Inputs = new Queue<int>(inputs);
            Outputs = new Queue<int>();
            ip = 0;
            Halted = false;
        }

        private int GetVal(int pos, Mode mode) => mode switch {
            Mode.Position => Memory[Memory[pos]],
            Mode.Immediate => Memory[pos],
        };

        private void SetVal(int pos, int value)
        {
            if(Memory.Length <= pos)
            {
                var temp = Memory;
                Array.Resize(ref temp, pos + 1);
                Memory = temp;
            }
            Memory[pos] = value;
        }

        private void Add(Mode mode1, Mode mode2)
        {
            SetVal(GetVal(ip+3, Mode.Immediate), GetVal(ip+1, mode1) + GetVal(ip+2, mode2));
            ip += 4;
        }

        private void Multiply(Mode mode1, Mode mode2)
        {
            SetVal(GetVal(ip+3, Mode.Immediate), GetVal(ip+1, mode1) * GetVal(ip+2, mode2));
            ip += 4;
        }

        private void Read()
        {
            SetVal(GetVal(ip+1, Mode.Immediate), Inputs.Dequeue());
            ip += 2;
        }

        private void Write(Mode mode)
        {
            Outputs.Enqueue(GetVal(ip+1, mode));
            ip += 2;
        }

        private void JumpIfTrue(Mode mode1, Mode mode2)
        {
            if(GetVal(ip+1, mode1) != 0)
                ip = GetVal(ip+2, mode2);
            else
                ip += 3;
        }

        private void JumpIfFalse(Mode mode1, Mode mode2)
        {
            if(GetVal(ip+1, mode1) == 0)
                ip = GetVal(ip+2, mode2);
            else
                ip += 3;
        }

        private void LessThan(Mode mode1, Mode mode2)
        {
            if(GetVal(ip+1, mode1) < GetVal(ip+2, mode2))
                SetVal(GetVal(ip+3, Mode.Immediate), 1);
            else
                SetVal(GetVal(ip+3, Mode.Immediate), 0);

            ip += 4;
        }

        private void EqualsInstruction(Mode mode1, Mode mode2)
        {
            if(GetVal(ip+1, mode1) == GetVal(ip+2, mode2))
                SetVal(GetVal(ip+3, Mode.Immediate), 1);
            else
                SetVal(GetVal(ip+3, Mode.Immediate), 0);

            ip += 4;
        }

        private void Halt() => Halted = true;
        
        private void Step()
        {
            var instruction = GetVal(ip, Mode.Immediate);
            var op = instruction % 100;
            Mode mode1 = (Mode) (instruction % 1000 / 100);
            Mode mode2 = (Mode) (instruction % 10000 / 1000);
            Mode mode3 = (Mode) (instruction % 100000 / 10000);

            switch(op) {
                case 1: Add(mode1, mode2); break;
                case 2: Multiply(mode1, mode2); break;
                case 3: Read(); break;
                case 4: Write(mode1); break;
                case 5: JumpIfTrue(mode1, mode2); break;
                case 6: JumpIfFalse(mode1, mode2); break;
                case 7: LessThan(mode1, mode2); break;
                case 8: EqualsInstruction(mode1, mode2); break;
                case 99: Halt(); break;
                default:
                    throw new Exception($"Unknown code: {op}");
            };
        }

        public int Run()
        {
            while(!Halted)
            {
                Step();
            }

            return GetVal(0, Mode.Immediate);
        }

        public void RunUntilWaitingInput()
        {
            while(!Halted && !(GetVal(ip, Mode.Immediate) == 3 && Inputs.Count == 0))
            {
                Step();
            }
        }
    }
}