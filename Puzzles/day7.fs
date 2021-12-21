namespace Puzzles

open FSharpPlus
open Puzzles
open Xunit

module Day7 =
    let input f = inputs.readAllLines "day7.txt" |> Seq.map (String.split [","] >> Seq.map f) |> Seq.head

    [<Fact>]
    let ``part1: actual input`` () =
        let sorted = input int64 |> sort |> Array.ofSeq
        let median = sorted[(sorted |> length) / 2]
        let result = sorted |> Seq.map (fun x -> (x - median) |> abs) |> Seq.sum
        Assert.Equal(336721L, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let sorted = input double |> sort |> Array.ofSeq

        let cost (n:double) = (n/2.) * (n+1.)
        let calc point =  sorted |> Seq.map (fun x -> (x - point) |> abs |> cost) |> Seq.sum
        
        let mean = sorted |> Seq.average       
        let result = 
            match calc (floor mean), calc (ceil mean) with
            | a, b when a < b -> a
            | _, b -> b

        Assert.Equal(91638945., result)
