namespace Puzzles

open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day6 =
    let input = inputs.ReadAllLines "day6.txt"
                |> Seq.map (String.split [","] >> Seq.map int)
                |> Seq.head
                |> Seq.fold (fun state x -> state |> List.updateAt x (state[x] + 1L)) (List.init 9 (fun _ -> 0L))

    let updateValueAt index valueFunc list = List.updateAt index (valueFunc (list |> List.item index)) list
        
    let run runs input =
        [1 .. runs]
        |> Seq.fold (fun (state: int64 list) _ -> (state[1..] |> updateValueAt 6 ((+) state[0])) @ [state[0]]) input
        |> Seq.sum
        
    let solve_1 input = run 80 input
        
    let solve_2 input = run 256 input