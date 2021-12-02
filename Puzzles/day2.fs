namespace Puzzles

open System.IO
open FSharpPlus
open Puzzles

module Day2 =
    type instruction =
        | Forward of int
        | Down of int
        | Up of int
        
    let input =
        seq {
            use stream = inputs.GetResourceStream("day2.txt")
            use reader = new StreamReader(stream)
            while not reader.EndOfStream do
                let line = reader.ReadLine()
                let instr, num = sscanf "%s %i" line
                yield match instr  with
                        | "forward" -> Forward num
                        | "down" -> Down num
                        | "up" -> Up num
                        | _ -> failwith $"unknown input %s{instr}"
        }

    type State = { Horizontal: int; Depth: int; Aim: int }
    
    let solve_1 input =
        input
            |> Seq.fold (fun state instruction ->
                       match instruction with
                           | Forward n -> { state with Horizontal = state.Horizontal + n }
                           | Down n -> { state with Depth = state.Depth + n }
                           | Up n -> { state with Depth = state.Depth - n }
                ) { Horizontal = 0; Depth = 0; Aim = 0}
            |> fun s -> s.Horizontal * s.Depth
    
    let solve_2 input =
        input
            |> Seq.fold (fun state instruction ->
                       match instruction with
                           | Forward n -> { state with Horizontal = state.Horizontal + n; Depth = state.Depth + (state.Aim * n) }
                           | Down n -> { state with Aim = state.Aim + n }
                           | Up n -> { state with Aim = state.Aim - n }
                ) { Horizontal = 0; Depth = 0; Aim = 0 }
        |> fun s -> s.Horizontal * s.Depth
