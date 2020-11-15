using System;
using System.Collections.Generic;

namespace day2cs
{
    public class IntCodeCPU
    {
        public int[] Memory { get; private set; }
        private int ip;
        private bool Halted { get; set; }

        public IntCodeCPU(int[] code)
        {
            Memory = code;
            ip = 0;
            Halted = false;
        }

        private int GetPosVal(int pos) =>
            Memory[GetVal(pos)];
        private int GetVal(int pos) =>
            Memory[pos];

        private void SetVal(int pos, int value) =>
            Memory[pos] = value;

        private void Add()
        {
            SetVal(GetVal(ip+3), GetPosVal(ip+1) + GetPosVal(ip+2));
            ip += 4;
        }

        private void Multiply()
        {
            SetVal(GetVal(ip+3), GetPosVal(ip+1) * GetPosVal(ip+2));
            ip += 4;
        }

        private void Halt() => Halted = true;
        
        private void Step()
        {
            switch(GetVal(ip)) {
                case 1: Add(); break;
                case 2: Multiply(); break;
                case 99: Halt(); break;
                default:
                    throw new Exception($"Unknown code: {GetVal(ip)}");
            };
        }

        public int Run()
        {
            while(!Halted)
            {
                Step();
            }

            return GetVal(0);
        }
    }
}