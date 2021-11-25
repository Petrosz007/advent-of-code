namespace Monorepo2020;

public static class LINQExtensions
{
    public static IEnumerable<T> ApplyAtIndex<T>(this IEnumerable<T> source, int index, Func<T,T> f) =>
        source.Select((x, i) => i == index ? f(x) : x);
    
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

    public static U FirstOrDefaultTransform<T, U>(this IEnumerable<T> source, Func<T, bool> predicate, Func<T, U> f,
        U defaultValue)
    {
        foreach (var item in source)
            if (predicate(item))
                return f(item);

        return default;
    }
}