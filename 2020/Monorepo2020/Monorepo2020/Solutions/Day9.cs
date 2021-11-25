namespace Monorepo2020.Solutions;

public class Day9 : SolutionBase<List<long>>
{
    public override List<long> ParseInput(IEnumerable<string> input) =>
        input.Select(long.Parse)
            .ToList();

    bool DoesntContainsSum((long Target, List<long> Premble) elem) =>
        !elem.Premble.Any(x => elem.Premble.Contains(elem.Target - x));
    
    public long Solve1(List<long> input, int prembleLength) =>
        input
            .Skip(prembleLength)
            .Select((x, i) => (x, input.GetRange(i, prembleLength)))
            .First(DoesntContainsSum)
            .Item1;

    public long Solve2(List<long> input, int prembleLength)
    {
        long target = Solve1(input, prembleLength);
        
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
    
    public override long Solve1(List<long> input) => Solve1(input, 25);
    public override long Solve2(List<long> input) => Solve2(input, 25);

}