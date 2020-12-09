using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

bool DoesntContainsSum((long Target, List<long> Premble) elem) =>
    !elem.Premble.Any(x => elem.Premble.Contains(elem.Target - x));

long Solve1(List<long> input, int prembleLenght) =>
    input
        .Skip(prembleLenght)
        .Select((x, i) => (x, input.GetRange(i, prembleLenght)))
        .First(DoesntContainsSum)
        .Item1;

long Solve2(List<long> input, int prembleLenght, long target)
{
    (long Sum, int Start, int Length) FindSequence(long elem, int i) => 
        input
            .Skip(i+1)
            .DoWhile(x => x.Item1 < target, (acc, x) => (acc.Item1 + x, i, acc.Item3 + 1), (elem, i, 1));

    (_, int start, int length) = input
        .Select(FindSequence)
        .First(x => x.Sum == target);

    var range = input.GetRange(start, length);
    return range.Min() + range.Max();
}


var input = File.ReadAllLines("input.txt")
    .Select(long.Parse)
    .ToList();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input, 25);
var part2 = Solve2(input, 25, part1);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");


public static class LINQExtension
{
    public static IEnumerable<U> Scan<T, U>(this IEnumerable<T> input, U state, Func<U, T, U> next) {
        yield return state;
        foreach(var item in input) {
            state = next(state, item);
            yield return state;
        }
    }

    public static U DoWhile<T, U>(this IEnumerable<T> input, Func<U, bool> predicate, Func<U, T, U> next, U state) {
        foreach(var item in input)
        {
            if(!predicate(state)) 
                return state;

            state = next(state, item);
        }
        return state;
    }
}
