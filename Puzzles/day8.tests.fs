namespace Puzzles

open Puzzles
open Xunit
open Day8

type ``Day 8 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(473, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(1097568, result)
       