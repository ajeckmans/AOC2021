namespace Puzzles

open Puzzles
open Xunit
open Day14

type ``Day 14 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 ()
        Assert.Equal(2010L, result)
       
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 ()
        Assert.Equal(2437698971143L, result)
