namespace Monorepo2020.Solutions;

public class Day3: SolutionBase<string[]>
{
    public override string[] ParseInput(IEnumerable<string> input) => input.ToArray();
    
    public static long CheckSlope(string[] input, int rightAmount, int downAmount)
    {
        var (row, col, treeCount) = (downAmount, rightAmount, 0);
        while(row < input.Length)
        {
            if(input[row][col] == '#') ++treeCount;

            col = (col + rightAmount) % input[row].Length;
            row += downAmount;
        }

        return treeCount;
    }

    public override long Solve1(string[] input) => 
        CheckSlope(input, 3, 1);

    public override long Solve2(string[] input) =>
        new []{ (1,1), (3,1), (5,1), (7,1), (1,2) }
            .Select(x => CheckSlope(input, x.Item1, x.Item2))
            .Aggregate((acc, x) => acc * x);
}