namespace Monorepo2020.Solutions;

public class Day8 : SolutionBase<(string Instruction, int Value)[]>
{
    public override (string Instruction, int Value)[] ParseInput(IEnumerable<string> input) =>
        input
            .Select(line => line.Replace("+", "").Split(" "))
            .Select(split => (split[0], int.Parse(split[1])))
            .ToArray();

    (int Accumulator, bool Finite) RunInstructions((string Instruction, int Value)[] input)
    {
        var (accumulator, ip, doneInstructions) = (0, 0, new HashSet<int>());
        while(!doneInstructions.Contains(ip))
        {
            if(ip == input.Length)
                return (accumulator, Finite: true);

            doneInstructions.Add(ip);
            (accumulator, ip) = input[ip] switch {
                ("acc", int x) => (accumulator + x, ip + 1),
                ("jmp", int x) => (accumulator,     ip + x),
                ("nop",     _) => (accumulator,     ip + 1),
            };
        }

        return (accumulator, Finite: false);
    }
    public override long Solve1((string Instruction, int Value)[] input) =>
        RunInstructions(input).Accumulator;
    
    (string Instruction, int Value)[] SwitchJmpNopAtId((string Instruction, int Value)[] input, int id) =>
        input[id].Instruction is "acc"
            ? input
            : input
                .ApplyAtIndex(id, instr => (input[id].Instruction is "jmp" ? "nop" : "jmp", instr.Value))
                .ToArray();

    public override long Solve2((string Instruction, int Value)[] input) =>
        input
            .Select((_, id) => SwitchJmpNopAtId(input, id))
            .Select(RunInstructions)
            .Single(result => result.Finite)
            .Accumulator;
}