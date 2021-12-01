namespace Puzzles

module Day1 =
    let solve_part1 input =
        
        input
            |> Seq.pairwise
            |> Seq.where (fun (a, b) -> b > a)
            |> Seq.length

    let solve_part2 (input: seq<int>) =
        input
            |> Seq.windowed 3
            |> Seq.pairwise
            |> Seq.where (fun (a, b) -> (Seq.sum b) > (Seq.sum a))
            |> Seq.length
