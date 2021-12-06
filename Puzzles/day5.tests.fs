namespace Puzzles

open Puzzles
open Xunit
open Day5

type ``Day 5 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(5774, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(18423, result)
       