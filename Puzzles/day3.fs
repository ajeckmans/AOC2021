namespace Puzzles

open System
open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day3 =
    let input = inputs.ReadAllLines "day3.txt" |> List.ofSeq

    let solve_1 input =      
        input
        |> List.map String.toList
        |> List.transpose
        |> List.map (fun x ->
            let total, numOnes = (List.length x, x |> List.where (fun item -> item = '1') |> List.length)
            if numOnes * 2 >= total then '1' else '0'
        )
        |> String.ofList
        |> fun s ->
            let mask = Convert.ToInt32(String.init s.Length (fun _ -> "1"), 2)
            let n = Convert.ToInt32(s, 2)
            (n &&& mask) * (~~~n &&& mask)        
        
    let solve_2 input =
       let rec findEntry bit_selection_criteria bit_position (input: string list) =
           let total, numOnes = input |> List.map (Seq.item bit_position) |> (fun x -> (List.length x, x |> List.where (fun item -> item = '1') |> List.length))
           let bitToSelectOn = if bit_selection_criteria (numOnes * 2) total then '1' else '0'
           let list = (input |> List.where (fun x -> x.Chars bit_position = bitToSelectOn ) )

           match list with
           | [] -> failwith "oops"
           | [item] -> item
           | rest -> findEntry bit_selection_criteria (bit_position+1) rest
                    
       let oxygenGeneratorRating = Convert.ToInt32(findEntry (>=) 0 input, 2)    
       let c02ScrubberRating = Convert.ToInt32(findEntry (<) 0 input, 2)
       
       oxygenGeneratorRating * c02ScrubberRating