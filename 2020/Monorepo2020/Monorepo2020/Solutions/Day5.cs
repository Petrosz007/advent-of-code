namespace Monorepo2020.Solutions;

public class Day5 : SolutionBase<string[]>
{
    public override string[] ParseInput(IEnumerable<string> input) => input.ToArray();
    
    public int BinaryLikeSearch(string input, int low, int high, char lowDelimiter)
    {
        foreach(char c in input[..^1])
        {
            if(c == lowDelimiter) 
                high = (low + high) / 2;
            else 
                low = (low + high) / 2 + 1;
        }
        int value = input.Last() == lowDelimiter ? low : high;
        return value;
    }

    public int SeatID(string input)
    {
        int row = BinaryLikeSearch(input[..7],   0, 127, 'F');
        int col = BinaryLikeSearch(input[7..10], 0,   7, 'L');

        return row * 8 + col;
    }

    public override long Solve1(string[] input) =>
        input.Select(SeatID).Max();

    public override long Solve2(string[] input)
    {
        var ids = input.Select(SeatID).OrderBy(x => x).ToList();
        for(int i = 0; i < ids.Count - 1; ++i)
            if(ids[i] + 1 != ids[i + 1])
                return ids[i] + 1;

        throw new Exception("Seat not found");
    }
}