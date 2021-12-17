namespace Puzzles

open FSharpPlus
open Puzzles
open Xunit

module Day2 =
    type instruction =
        | Forward of int
        | Down of int
        | Up of int
    
    let input = inputs.ReadAllLines "day2.txt"
                |> Seq.map (sscanf "%s %i" >> fun (instr, num) ->
                        match instr with
                        | "forward" -> Forward num
                        | "down" -> Down num
                        | "up" -> Up num
                        | _ -> failwith $"unknown input %s{instr}" )
                |> List.ofSeq

    type State = { Horizontal: int; Depth: int; Aim: int }
    
    [<Fact>]
    let ``part1: actual input`` () =
        let result =
            input
            |> Seq.fold (fun state instruction ->
                   match instruction with
                       | Forward n -> { state with Horizontal = state.Horizontal + n }
                       | Down n -> { state with Depth = state.Depth + n }
                       | Up n -> { state with Depth = state.Depth - n }
                ) { Horizontal = 0; Depth = 0; Aim = 0}
            |> fun s -> s.Horizontal * s.Depth
        Assert.Equal(1648020, result)
  
    let ``part2: actual input`` () =
        let result =
            input
            |> Seq.fold (fun state instruction ->
                       match instruction with
                           | Forward n -> { state with Horizontal = state.Horizontal + n; Depth = state.Depth + (state.Aim * n) }
                           | Down n -> { state with Aim = state.Aim + n }
                           | Up n -> { state with Aim = state.Aim - n }
                ) { Horizontal = 0; Depth = 0; Aim = 0 }
            |> fun s -> s.Horizontal * s.Depth

        Assert.Equal(1759818555, result)
