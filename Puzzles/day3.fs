namespace Puzzles

open System
open System.IO
open FSharpPlus
open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day3 =
    let input =
        seq {
            use stream = inputs.GetResourceStream("day3.txt")
            use reader = new StreamReader(stream)
            while not reader.EndOfStream do
                let line = reader.ReadLine()
                yield line
        }

    let solve_1 (input:seq<string>) =      
       seq {
            let chars = input |> Seq.map (fun x -> x |> String.toArray)
                       
            for i in 0 .. (Seq.head chars).Length - 1 do
               let total, numOnes = chars |> Seq.map (Seq.item i) |> (fun x -> (Seq.length x, x |> Seq.where (fun item -> item = '1') |> Seq.length))
               yield if (decimal numOnes) >= ((decimal total) / 2M) then '1' else '0'
       } |> String.ofSeq
       |> fun s ->
            let mask = Convert.ToInt32(String.init s.Length (fun _ -> "1"), 2)
            let n = Convert.ToInt32(s, 2)
            (n &&& mask, ~~~n &&& mask)
       |> (fun (a,b) -> b * a)
        
        
    let solve_2 (input: string list) =
       let findEntry bit_criteria (input: string list) =
           let mutable list = input
           let mutable pos = 0
           while list.Length > 1 || list.Length > 1 do
               let total, numOnes = list |> Seq.map (Seq.item pos) |> (fun x -> (Seq.length x, x |> Seq.where (fun item -> item = '1') |> Seq.length))
               let mostCommon = if bit_criteria (decimal numOnes) ((decimal total) / 2M) then '1' else '0'
               list <- (list |> List.where (fun x -> x.Chars pos = mostCommon ) )
               pos <- pos + 1
           List.head list
              
       let oxygenGeneratorRating = Convert.ToInt32(findEntry (>=) input, 2)    
       let c02ScrubberRating = Convert.ToInt32(findEntry (<) input, 2)
       
       oxygenGeneratorRating * c02ScrubberRating