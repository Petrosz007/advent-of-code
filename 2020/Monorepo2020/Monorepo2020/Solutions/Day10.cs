namespace Monorepo2020.Solutions;

public class Day10 : SolutionBase<int[]>
{
    public override int[] ParseInput(IEnumerable<string> input) =>
        input.Select(int.Parse)
            .ToArray();

    public override long Solve1(int[] input) =>
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

    public override long Solve2(int[] input)
    {
        var adapters = input.OrderBy(x => x).Prepend(0).ToList();
        adapters.Add(adapters.Max() + 3);

        var dp = new Dictionary<int,long>();
        dp[adapters.Max()] = 1;

        long CalcDp(int num) =>
            dp.ContainsKey(num)
                ? dp[num]
                : dp[num] = adapters
                    .Where(x => num < x && x <= num + 3)
                    .Select(CalcDp)
                    .Sum();

        return CalcDp(0);
    }
}