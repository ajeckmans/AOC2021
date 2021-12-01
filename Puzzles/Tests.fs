namespace Puzzles

open Puzzles
open Xunit

type ``Day 1``() =
   
    [<Fact>]
    let ``part1: sample input`` () =
        let input = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263|]
        let result = Day1.solve_part1 input
        Assert.Equal(7, result)
        
    [<Fact>]
    let ``part1: actual input`` () =
        let result = Day1.solve_part1 Day1.input
        Assert.Equal(1532, result)
        
    [<Fact>]
    let ``part2: sample input`` () =
        let input = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263|]
        let result = Day1.solve_part2 input
        Assert.Equal(5, result)
        
    [<Fact>]
    let ``part2: actual input`` () =
        let result = Day1.solve_part2 Day1.input
        Assert.Equal(1571, result)