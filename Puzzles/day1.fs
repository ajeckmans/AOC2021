namespace Puzzles

open System.IO

module Day1 =
    let input =
        seq {
            use stream = inputs.GetResourceStream("day1.txt")
            use reader = new StreamReader(stream)
            while not reader.EndOfStream do
                yield reader.ReadLine() |> int
        }

    let countIncreasing map input =
        input
            |> Seq.pairwise
            |> Seq.where (fun (a, b) -> map b > map a)
            |> Seq.length
    
    let solve_part1 input =
        input
        |> countIncreasing id

    let solve_part2 input =
        input
        |> Seq.windowed 3
        |> countIncreasing (Seq.reduce (+))
