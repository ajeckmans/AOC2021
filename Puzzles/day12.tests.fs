namespace Puzzles

open Puzzles
open Xunit
open Day12

type ``Day 12 Tests`` () =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 ()
        Assert.Equal(3576, result)
       
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 ()
        Assert.Equal(84271, result)
