namespace Puzzles

open System
open System.IO
open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day7 =
    let input =
        use stream = inputs.GetResourceStream("day7.txt")
        use reader = new StreamReader(stream)
        reader.ReadLine()
            |> String.split [","]
            |> Seq.map int64

    let solve_1 (input: seq<int64>) =
        let sorted = input |> sort |> Array.ofSeq
        let median = sorted.[(sorted |> length) / 2]
        sorted |> Seq.map (fun x -> (x - median) |> abs) |> Seq.sum
        
    let solve_2 input =
        let cost n = if n = 0L then 0L else [1L..n] |> List.reduce (+)
        
        let rec find list lowest position : int64 =
            match list |> Seq.map (fun x -> cost (x - position |> abs)) |> Seq.sum with
            | cost when cost > lowest -> lowest
            | cost when cost < lowest -> find list cost (position+1L)
            | _ -> find list lowest (position+1L)
        
        find input Int64.MaxValue 0L
