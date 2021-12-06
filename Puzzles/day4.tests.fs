namespace Puzzles

open Puzzles
open Xunit

type ``Day 4 Tests``() =
    let input = Day4.input
   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = Day4.solve_1 input
        Assert.Equal(Some 51034, result)
        
        
    [<Fact>]
    let ``part2: actual input`` () =
        let result = Day4.solve_2 input
        Assert.Equal(Some 5434, result)