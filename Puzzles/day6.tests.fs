namespace Puzzles

open Puzzles
open Xunit
open Day6

type ``Day 6 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(371379L, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(1674303997472L, result)
       