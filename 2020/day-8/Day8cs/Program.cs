using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

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

int Solve1((string Instruction, int Value)[] input) =>
    RunInstructions(input).Accumulator;

(string Instruction, int Value)[] SwitchJmpNopAtId((string Instruction, int Value)[] input, int id) =>
    input[id].Instruction is "acc"
        ? input
        : input
            .ApplyAtIndex(id, instr => (input[id].Instruction is "jmp" ? "nop" : "jmp", instr.Value))
            .ToArray();

int Solve2((string Instruction, int Value)[] input) =>
    input
        .Select((_, id) => SwitchJmpNopAtId(input, id))
        .Select(RunInstructions)
        .Single(result => result.Finite)
        .Accumulator;

var input = File.ReadAllLines("input.txt")
    .Select(line => line.Replace("+", "").Split(" "))
    .Select(split => (split[0], int.Parse(split[1])))
    .ToArray();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input);
var part2 = Solve2(input);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");


public static class LINQExtension
{
    public static IEnumerable<T> ApplyAtIndex<T>(this IEnumerable<T>? source, int index, Func<T,T> f) =>
        source.Select((x, i) => i == index ? f(x) : x);
}
