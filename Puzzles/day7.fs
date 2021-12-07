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

    let rec find list lowest position costFunction : int64 =
        match list |> Seq.map (fun x -> (costFunction x position)) |> Seq.sum with
        | cost when cost > lowest -> lowest
        | cost when cost < lowest -> find list cost (position+1L) costFunction
        | _ -> find list lowest (position+1L) costFunction
        
    let solve_1 input =
        find input Int64.MaxValue 0L (fun x position -> x - position |> abs)
        
    let solve_2 input =
        let cost n = if n = 0L then 0L else [1L..n] |> List.reduce (+)
        find input Int64.MaxValue 0L (fun x position -> cost (x - position |> abs))
