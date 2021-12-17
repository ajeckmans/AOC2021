namespace Puzzles

open System
open System.Collections.Generic
open System.IO
open System.Text
open Puzzles
open FSharpPlus
open Xunit

module Day14 =
    let input () =
        use stream = inputs.GetResourceStream("day14.txt")
        use reader = new StreamReader(stream)

        let sequence = reader.ReadLine()
        reader.ReadLine() |> ignore

        let insertionRules = 
            seq {
                while not reader.EndOfStream do
                    yield (reader.ReadLine() |> sscanf "%s -> %s" )
            } |> Map.ofSeq
        
        sequence, insertionRules
       
    [<Fact>]
    let ``part1: actual input`` () =
       let sequence, insertionRules = input ()
       
       let charCounts =
           [1..10]
           |> Seq.fold (fun (state: string) _ ->
                       let builder = StringBuilder(state.Substring(0, 1), state.Length + state.Length )
                       state
                       |> Seq.pairwise
                       |> Array.ofSeq
                       |> Array.Parallel.map(fun (a, b) ->
                           match insertionRules.TryFind($"%c{a}%c{b}") with
                           | Some rule -> $"%s{rule}%c{b}"
                           | None -> $"%c{b}"
                       )
                       |> Array.fold (fun (sb:StringBuilder) -> sb.Append) builder
                       |> fun x -> x.ToString()
                   ) sequence
           |> Seq.countBy id
           |> Seq.map snd
           |> Seq.sortDescending
           
       let highest = charCounts |> Seq.head 
       let lowest = charCounts |> Seq.last 
       
       let result = highest - lowest
       
       Assert.Equal(2010L, result)
              
    [<Fact>]
    let ``part2: actual input`` () =
        let sequence, insertionRules = input ()
       
        let sequenceCount =
           sequence
           |> Seq.pairwise
           |> Seq.map (fun (a,b) -> string a + string b)
           |> Seq.countBy id
           |> Seq.map (fun (a, b) -> (a, b |> int64))
           |> Map.ofSeq
           
        let initialCount =  Map.union sequenceCount (insertionRules |> Map.mapValues (fun _ -> 0))

        let ifTrue f o = if Option.isSome o then Some(f o.Value) else None

        let counted =
           [1..40]
           |> List.fold ( fun (state: Map<string, int64>) _ ->
                Seq.fold (fun intermediate (kvp: KeyValuePair<string, int64>)  ->
                           match insertionRules.TryFind(kvp.Key) with
                           | Some rule ->
                               if kvp.Value > 0L then 
                                   let right = $"%s{rule}%c{kvp.Key.Chars 1}"
                                   let left = $"%c{kvp.Key.Chars 0}%s{rule}"
                                   ((intermediate.Change (kvp.Key, ifTrue (fun x -> x - kvp.Value)))
                                        .Change (left, ifTrue (fun x -> x + kvp.Value)))
                                        .Change (right, ifTrue (fun x -> x + kvp.Value))
                               else intermediate
                           | None -> intermediate
                ) state state
               ) initialCount

        let firstChar = sequence.Chars 0
        let charCounts =
           counted
           |> Seq.map (fun x -> x.Key.Chars 1, x.Value)
           |> List.ofSeq
           |> List.append [firstChar, 1]
           |> List.groupBy fst
           |> List.map (fun (_, list) -> list |> Seq.sumBy snd)

        let result = 
           charCounts
           |> List.fold (fun (highest, lowest) elem ->
               (if elem > highest then elem else highest), (if elem < lowest then elem else lowest))
               (0L,Int64.MaxValue)
           |> fun (highest, lowest) ->  highest - lowest
       
        Assert.Equal(2437698971143L, result)
