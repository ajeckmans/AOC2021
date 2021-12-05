namespace Puzzles

open Puzzles
open Xunit
open Day4

type ``Day 4 Tests``() =
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1
        Assert.Equal(51034, result)
        
        
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2
        Assert.Equal(5434, result)