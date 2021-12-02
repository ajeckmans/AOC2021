namespace Puzzles

open Puzzles
open Xunit
open Day2

type ``Day 2 Tests``() =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(1648020, result)

    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(1759818555, result)
