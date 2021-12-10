namespace Puzzles

module Day1 =
    let input = inputs.ReadAllLines "day1.txt" |> Seq.map int

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