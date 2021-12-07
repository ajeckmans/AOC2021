namespace Puzzles

open Puzzles
open Xunit
open Day7

type ``Day 7 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 (Seq.map int64 input)
        Assert.Equal(336721L, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 (input |> Seq.map double)
        Assert.Equal(91638945., result)
       