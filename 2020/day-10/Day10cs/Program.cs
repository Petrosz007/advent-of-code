using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

int Solve1(int[] input) =>
    input
        .OrderBy(x => x)
        .Aggregate(
            (Prev: 0, Ones: 0,Threes: 1),
            (acc, x) => (x - acc.Prev) switch {
                1 => (x, acc.Ones + 1, acc.Threes    ),
                3 => (x, acc.Ones,     acc.Threes + 1),
                _ => (x, acc.Ones,     acc.Threes    ),
            },
            acc => acc.Ones * acc.Threes
        );

long Solve2(int[] input)
{
    var adapters = input.OrderBy(x => x).Prepend(0).ToList();
    adapters.Add(adapters.Max() + 3);

    var dp = new Dictionary<int,long>();
    dp[adapters.Max()] = 1;

    long calcDp(int num) =>
        dp.ContainsKey(num)
            ? dp[num]
            : dp[num] = adapters
                .Where(x => num < x && x <= num + 3)
                .Select(calcDp)
                .Sum();

    return calcDp(0);
}


var input = File.ReadAllLines("input.txt")
    .Select(int.Parse)
    .ToArray();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input);
var part2 = Solve2(input);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");
