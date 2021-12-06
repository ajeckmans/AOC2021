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
            |> Seq.groupBy id
            |> Seq.sortBy fst
            |> Seq.map (fun (_, list) -> length list |> int64)
            |> List.ofSeq
            |> (fun x -> (x @ [0L;0L;0L;0L;0L;0L;0L;0L;0L]).[0..8])

    let run runs input =
        [1 .. runs - 1]
        |> Seq.fold (fun state _ -> List.map2 (+) (state.[1..] @ [0L]) [0L;0L;0L;0L;0L;0L;state.[0];0L;state.[0]]) input
        |> Seq.sum
        
    let solve_1 input =
        run 80 input
        
    let solve_2 input = 
        run 256 input
