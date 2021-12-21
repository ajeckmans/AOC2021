namespace Puzzles

open FSharpPlus
open Puzzles
open Xunit

module Day6 =
    let input = inputs.readAllLines "day6.txt"
                |> Seq.map (String.split [","] >> Seq.map int)
                |> Seq.head
                |> Seq.fold (fun state x -> state |> List.updateAt x (state[x] + 1L)) (List.init 9 (fun _ -> 0L))

    let updateValueAt index valueFunc list = List.updateAt index (valueFunc (list |> List.item index)) list
        
    let run runs input =
        [1 .. runs]
        |> Seq.fold (fun (state: int64 list) _ -> (state[1..] |> updateValueAt 6 ((+) state[0])) @ [state[0]]) input
        |> Seq.sum
        
    [<Fact>]
    let ``part1: actual input`` () =
        let result = run 80 input
        Assert.Equal(371379L, result)
       
    [<Fact>]
    let ``part2: actual input`` () =
        let result = run 256 input
        Assert.Equal(1674303997472L, result)
       