namespace Puzzles

open System.IO
open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day6 =
    let input =
        use stream = inputs.GetResourceStream("day6.txt")
        use reader = new StreamReader(stream)
        reader.ReadLine()
            |> String.split [","]
            |> Seq.map int
            |> List.ofSeq
            |> List.groupBy id
            |> List.map (fun (x,list) -> x, length list |> int64)

        
    let updateList fishes =
        fishes
        |> List.mapFold (fun state (nextProduction, amount) ->
                match nextProduction - 1 with
                | -1 -> (6, amount), state + amount
                | fish -> (fish, amount), state 
            ) 0L
    
    let condense list =
        list
        |> List.groupBy fst
        |> List.map (fun (x,list) -> x, list |> Seq.sumBy snd)
            
    let solve_1 input =
        [1 .. 80]
        |> Seq.fold (fun state _ ->
            let  updatedFishes, newFishes = state |> updateList
            (updatedFishes @ [(8, newFishes)]) |> condense
        ) input
        |> Seq.sumBy snd

    let solve_2 input = 
        [1 .. 256]
        |> Seq.fold (fun state _ ->
            let  updatedFishes, newFishes = state |> updateList
            updatedFishes @ [8, newFishes]
        ) input
        |> Seq.sumBy snd
