using System.Threading;
using System.Diagnostics;
using System.Linq;
using System.IO;
using System;
using day2cs;

int RunNounVerb(int[] code, int noun, int verb)
{
    int[] localCode = (int[])code.Clone();
    localCode[1] = noun;
    localCode[2] = verb;
    var cpu = new IntCodeCPU(localCode);
    var result = cpu.Run();
    return result;
}

int SolvePart1(int[] code) => RunNounVerb(code, 12, 2);

int SolvePart2(int[] code, int target)
{
    for(int noun = 0; noun <= 99; ++noun)
        for(int verb = 0; verb <= 99; ++verb)
            if(target == RunNounVerb(code, noun, verb))
                return 100 * noun + verb;

    throw new Exception("Target could not be found");
}

var code = File.ReadAllText("input.txt").Split(',').Select(x => int.Parse(x)).ToArray();

var stopwatch = Stopwatch.StartNew();

var part1 = SolvePart1(code);
var part2 = SolvePart2(code, 19690720);

stopwatch.Stop();

Console.WriteLine($"Solved in {stopwatch.ElapsedMilliseconds}ms");
Console.WriteLine($"Part 1: {part1}");
Console.WriteLine($"Part 2: {part2}");