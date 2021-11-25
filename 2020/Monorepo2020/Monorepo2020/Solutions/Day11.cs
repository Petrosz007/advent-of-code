namespace Monorepo2020.Solutions;


public class Day11 : SolutionBase<char[,]>
{
    delegate IEnumerable<char> NeighboursDelegate(char[,] board, int row, int col);
    
    public override char[,] ParseInput(IEnumerable<string> input) =>
        input.Select(x => x.AsEnumerable().ToArray())
            .ToArray()
            .ToMatrix();

    IEnumerable<char> NeighboursPart1(char[,] board, int row, int col)
    {
        for (int i = Math.Max(0, row - 1); i <= Math.Min(board.GetLength(0) - 1, row + 1); ++i)
        for (int j = Math.Max(0, col - 1); j <= Math.Min(board.GetLength(1) - 1, col + 1); ++j)
            if (!(i == row && j == col))
                yield return board[i, j];
    }

    char[,] Step(char[,] board, NeighboursDelegate neighbours, int emptyCutoff)
    {
        var newBoard = (char[,])board.Clone();

        for (int i = 0; i < board.GetLength(0); ++i)
        for (int j = 0; j < board.GetLength(1); ++j)
            newBoard[i, j] = board[i, j] switch
            {
                'L' => neighbours(board, i, j).All(c => c is not '#')
                    ? '#'
                    : 'L',
                '#' => neighbours(board, i, j).Count(c => c is '#') >= emptyCutoff
                    ? 'L'
                    : '#',
                char c => c,
            };

        return newBoard;
    }

    int Solve(char[,] input, NeighboursDelegate neighbours, int emptyCutoff) =>
        LINQExtensions.DoWhile(
                x => !LINQExtensions.MatrixEquals(x.Prev, x.Stepped),
                x => (x.Stepped, Step(x.Stepped, neighbours, emptyCutoff)),
                (Prev: input, Stepped: Step(input, neighbours, emptyCutoff)))
            .Stepped
            .Cast<char>().Count(x => x is '#');

    char GetNeighbourInDirection(char[,] board, int row, int col, Func<int, int, (int i, int j)> direction) =>
        LINQExtensions.ScanWhile(
                x => board.IndexesInRange(x.i, x.j),
                x => direction(x.i, x.j),
                direction(row, col))
            .FirstOrDefaultTransform(x => board[x.i, x.j] is not '.', x => board[x.i, x.j], '.');

    IEnumerable<char> NeighboursPart2(char[,] board, int row, int col) =>
        new Func<int, int, (int i, int j)>[]
            {
                (i, j) => (i, j + 1),
                (i, j) => (i, j - 1),
                (i, j) => (i + 1, j),
                (i, j) => (i + 1, j + 1),
                (i, j) => (i + 1, j - 1),
                (i, j) => (i - 1, j),
                (i, j) => (i - 1, j + 1),
                (i, j) => (i - 1, j - 1),
            }
            .Select(f => GetNeighbourInDirection(board, row, col, f));

    public override long Solve1(char[,] input) => Solve(input, NeighboursPart1, 4);

    public override long Solve2(char[,] input) => Solve(input, NeighboursPart2, 5);
}