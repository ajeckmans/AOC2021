namespace Puzzles

open FSharpPlus
open Puzzles

module Day7 =
    let input = inputs.ReadAllLines "day7.txt" |> Seq.map (String.split [","] >> Seq.map int64) |> Seq.head

    let solve_1 (input: seq<int64>) =
        let sorted = input |> sort |> Array.ofSeq
        let median = sorted[(sorted |> length) / 2]
        sorted |> Seq.map (fun x -> (x - median) |> abs) |> Seq.sum

    let solve_2 (input:seq<double>) =
        let sorted = input |> sort |> Array.ofSeq

        let cost (n:double) = (n/2.) * (n+1.)
        let calc point =  sorted |> Seq.map (fun x -> (x - point) |> abs |> cost) |> Seq.sum
        
        let mean = sorted |> Seq.average       
        match calc (floor mean), calc (ceil mean) with
        | a, b when a < b -> a
        | _, b -> b
