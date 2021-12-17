namespace Puzzles

open Xunit

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
        
    [<Fact>]
    let ``part1: sample input`` () =
        let input = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263|]
        let result = solve_part1 input
        Assert.Equal(7, result)
        
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_part1 input
        Assert.Equal(1532, result)
        
    [<Fact>]
    let ``part2: sample input`` () =
        let input = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263|]
        let result = solve_part2 input
        Assert.Equal(5, result)
        
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_part2 input
        Assert.Equal(1571, result)