namespace Puzzles

open Puzzles
open Xunit
open Day10

type ``Day 10 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        
        Assert.Equal(388713, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(3539961434L, result)
       