namespace Puzzles

open Puzzles
open Xunit
open Day3

type ``Day 3 Tests``() =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(2595824, result)

    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(2135254, result)
