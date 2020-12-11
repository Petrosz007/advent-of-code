using System.Collections;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;


IEnumerable<char> NeighboursPart1(char[,] board, int row, int col)
{
    for(int i = Math.Max(0, row-1); i <= Math.Min(board.GetLength(0)-1, row+1); ++i)
        for(int j = Math.Max(0, col-1); j <= Math.Min(board.GetLength(1)-1, col+1); ++j)
            if(!(i == row && j == col))
                yield return board[i,j];
}

char[,] Step(char[,] board, NeighboursDelegate neighbours, int emptyCutoff)
{
    var newBoard = (char[,]) board.Clone();

    for(int i = 0; i < board.GetLength(0); ++i)
        for(int j = 0; j < board.GetLength(1); ++j)
            newBoard[i,j] = board[i,j] switch {
                'L' => neighbours(board, i, j).All(c => c is not '#')
                        ? '#' : 'L',
                '#' => neighbours(board, i, j).Where(c => c is '#').Count() >= emptyCutoff
                        ? 'L' : '#',
                char c => c,
            };

    return newBoard;
}

int Solve(char[,] input, NeighboursDelegate neighbours, int emptyCutoff) =>
    Extensions.DoWhile(
            x => !Extensions.MatrixEquals(x.Prev, x.Stepped),
            x => (x.Stepped, Step(x.Stepped, neighbours, emptyCutoff)),
            (Prev: input, Stepped: Step(input, neighbours, emptyCutoff)))
        .Stepped
        .Cast<char>().Count(x => x is '#');

char GetNeighbourInDirection(char[,] board, int row, int col, Func<int, int, (int i, int j)> direction) =>
    Extensions.ScanWhile(
        x => board.IndexesInRange(x.i, x.j),
        x => direction(x.i, x.j),
        direction(row, col))
    .FirstOrDefaultTransform(x => board[x.i, x.j] is not '.', x => board[x.i, x.j], '.');

IEnumerable<char> NeighboursPart2(char[,] board, int row, int col) =>
    new Func<int, int, (int i, int j)>[]{
        (i,j) => (i,j+1),
        (i,j) => (i,j-1),
        (i,j) => (i+1,j),
        (i,j) => (i+1,j+1),
        (i,j) => (i+1,j-1),
        (i,j) => (i-1,j),
        (i,j) => (i-1,j+1),
        (i,j) => (i-1,j-1),
    }
    .Select(f => GetNeighbourInDirection(board, row, col, f));

int Solve1(char[,] input) =>
    Solve(input, NeighboursPart1, 4);

int Solve2(char[,] input) =>
    Solve(input, NeighboursPart2, 5);


char[,] input = File.ReadAllLines("input.txt")
    .Select(x => x.AsEnumerable().ToArray())
    .ToArray()
    .ToMatrix();

var stopwatch = Stopwatch.StartNew();

var part1 = Solve1(input);
var part2 = Solve2(input);

stopwatch.Stop();

Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
Console.WriteLine($"Part1: {part1}");
Console.WriteLine($"Part2: {part2}");


delegate IEnumerable<char> NeighboursDelegate(char[,] board, int row, int col);

public static class Extensions
{
    public static T[,] ToMatrix<T>(this T[][] source)
    {
        if(source.Length == 0)
            return new T[0,0];

        var lengths = source.Select(arr => arr.Length);
        if(lengths.Min() != lengths.Max())
            throw new ArgumentException("Cannot convert jagged array with multiple lengths to 2d matrix");

        var innerLength = lengths.First();

        var matrix = new T[source.Length, innerLength];
        for(int i = 0; i < source.Length; ++i)
            for(int j = 0; j < innerLength; ++j)
                matrix[i,j] = source[i][j];

        return matrix;
    }

    public static bool MatrixEquals<T>(T[,] left, T[,] right)
    {
        var a = left.Cast<T>();
        var b = right.Cast<T>();
        return a.SequenceEqual(b);
    }

    public static bool IndexesInRange<T>(this T[,] source, int i, int j)
    {
        return i >= 0 && i < source.GetLength(0)
            && j >= 0 && j < source.GetLength(1);
    }

    public static IEnumerable<U> ScanWhile<U>(Func<U, bool> predicate, Func<U, U> next, U state) {
        while(predicate(state)) 
        {
            yield return state;
            state = next(state);
        }
    }

    public static U DoWhile<U>(Func<U, bool> predicate, Func<U, U> next, U state) {
        while(predicate(state)) 
        {
            state = next(state);
        }
        return state;
    }

    public static U FirstOrDefaultTransform<T, U>(this IEnumerable<T> source, Func<T, bool> predicate, Func<T, U> f, U defaultValue)
    {
        foreach(var item in source)
            if(predicate(item))
                return f(item);

        return defaultValue;
    }
}
