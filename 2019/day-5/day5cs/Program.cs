using System.Threading;
using System.Diagnostics;
using System.Linq;
using System.IO;
using System;
using day5cs;

int Solve(int[] code, int input)
{
    int[] localCode = (int[])code.Clone();
    var cpu = new IntCodeCPU(localCode, new int[]{input});
    cpu.Run();
    
    return cpu.Outputs.Last();
}

var code = File.ReadAllText("input.txt").Split(',').Select(x => int.Parse(x)).ToArray();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve(code, 1);
var part2 = Solve(code, 5);

stopwatch.Stop();

Console.WriteLine($"Solved in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part 1: {part1}");
Console.WriteLine($"Part 2: {part2}");
