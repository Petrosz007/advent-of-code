

open System.IO

type Compare<'a> = int * 'a[] -> int * 'a[] -> int * 'a[]

let indexes (xs: 'a[]) = [| for i in 0..(xs.Length - 1) -> i |]

let binaryToDec xs =
    Array.foldBack (fun i (sum, power) -> (sum + i * (pown 2 power), power + 1)) xs (0, 0)
    |> fst
    
let selectColumn (xs : 'a[][]) i =
    xs |> Array.map (fun y -> y[i])
    
let transpose (xs : 'a[][]) =
    xs[0] |> indexes
    |> Array.map (selectColumn xs)

let getElementBy (compare : Compare<'a>) xs =
    xs
    |> Array.groupBy id
    |> Array.reduce compare
    |> fst
    
let selectBitsBy (compare : Compare<'a>) xs =
    xs
    |> transpose
    |> Array.map (getElementBy compare)
    
let myCompare target f (x, xs: 'a[]) (y, ys: 'a[]) =
    if xs.Length = ys.Length
    then if x = target then (x, xs) else (y, ys)
    else if f xs.Length ys.Length
         then (x, xs)
         else (y, ys)
         
let myMaxCompare = myCompare 1 (fun x y -> x > y)
let myMinCompare = myCompare 0 (fun x y -> x < y)

let solve1 input =
    let max = selectBitsBy myMaxCompare input |> binaryToDec
    let min = selectBitsBy myMinCompare input |> binaryToDec
    max * min
    
let filterBitsBy compare input i =
    let selectedBits = selectBitsBy compare input
    input
    |> Array.filter (fun xs -> selectedBits[i] = xs[i])
    
let iterateFilterBitsBy (compare: Compare<'a>) (input: int[][]): int[] =
    input[0] |> indexes
    |> Array.fold (filterBitsBy compare) input
    |> Array.head

let solve2 input =
    let max = iterateFilterBitsBy myMaxCompare input |> binaryToDec
    let min = iterateFilterBitsBy myMinCompare input |> binaryToDec
    max * min

let inputNumbers =
    File.ReadAllLines("input.txt")
    |> Array.map ((>>) Seq.toArray (Array.map (fun c -> string c |> int)))

let part1 = solve1 inputNumbers
let part2 = solve2 inputNumbers

printfn $"Part 1: {part1}"
printfn $"Part 1: {part2}"
printfn $"Is the solution correct? {part1 = 1458194 && part2 = 2829354}"
