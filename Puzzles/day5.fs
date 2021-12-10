namespace Puzzles

open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day5 =
    let input = inputs.ReadAllLines "day5.txt"
                |> Seq.map (sscanf "%d,%d -> %d,%d" >> fun (ax, ay, bx, by) -> if  ax < bx then (ax, ay), (bx, by) else (bx, by), (ax, ay))
                |> List.ofSeq

    let getLineCoords line =
        let getNextCoordinate line (x,y) =
            match line with
            | (ax, ay), (bx, by) when ax < bx && ay = by -> (x + 1, y)
            | (ax, ay), (bx, by) when ax > bx && ay = by -> (x + -1, y)
            | (ax, ay), (bx, by) when ay < by && ax = bx -> (x, y + 1)
            | (ax, ay), (bx, by) when ay > by && ax = bx -> (x, y + -1)
            | (_, ay), (_, by) when ay < by -> (x + 1, y + 1)
            | (_, ay), (_, by) when ay > by -> (x + 1, y + -1)
            | _ -> failwith "unsupported"

        let endCoordinate =  getNextCoordinate line (snd line)
        
        fst line
        |> List.unfold (
            fun current ->
                let nextCoordinate = getNextCoordinate line current
                if current = endCoordinate then None
                else Some (current, nextCoordinate)   
            )
    
    let getIntersectionCount lines =
        lines
        |> List.collect getLineCoords
        |> List.groupBy id
        |> List.where (fun (_, list) -> List.length list > 1)
        |> List.length
        
    let solve_1 input =
        input
        |> List.where (fun ((ax, ay), (bx, by)) -> ax = bx || ay = by)
        |> getIntersectionCount
        
    let solve_2 input =
        input
        |> getIntersectionCount