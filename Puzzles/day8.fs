namespace Puzzles

open System
open FSharpPlus
open Puzzles
open Xunit

module Day8 =
    let input = inputs.readAllLines "day8.txt"
                |> Seq.map ( sscanf "%s | %s" >> (fun (signal, output) -> String.split [" "] signal, String.split [" "] output ))
       
       
       
    [<Fact>]
    let ``part1: actual input`` () =
        let lengthToNumber length =
            match length with
            | 2 -> Some 1
            | 3-> Some 7
            | 4 -> Some  4
//            | 5 -> 2 | 3 | 5 
//            | 6 -> 0 | 6 | 9
            | 7 -> Some 8
            | _ -> None
        
        let result =
            input
            |> Seq.map snd
            |> Seq.collect id
            |> Seq.map (fun x -> x |> length  |> lengthToNumber)
            |> Seq.sumBy Option.count

        Assert.Equal(473, result)

    [<Fact>]
    let ``part2: actual input`` () =
        let result =
            input
            |> Seq.map (fun x -> fst x |> Seq.map sort |> Array.ofSeq, snd x |> Array.ofSeq)
            |> Seq.sumBy (fun (left, output) ->
                let one = left |> Seq.find (length >> (=) 2)
                let four = left |> Seq.find (length >> (=) 4)
                let seven = left |> Seq.find (length >> (=) 3)
                let eight = left |> Seq.find (length >> (=) 7)
                          
                let elementsWithLengthSix = left |> Array.filter (length >> (=) 6)              
                let six = elementsWithLengthSix |> Array.find (fun e -> one |> Seq.except e |> length = 1)
                let remaining = elementsWithLengthSix |> Array.filter ((<>) six)
                let nine = remaining |> Array.find (fun e -> four |> Seq.except e |> length = 0)
                let zero = remaining |> Array.find ((<>) nine)

                let elementsWithLengthFive = left |> Array.filter (length >> (=) 5)              
                let three = elementsWithLengthFive|> Array.find (Seq.except one >> length >> (=) 3)
                let remaining = elementsWithLengthFive |> Array.filter ((<>) three)
                let two =  remaining |> Array.find (Seq.except six >> length >> (=) 1)
                let five = remaining |> Array.find ((<>) two)
                
                let mapping = [|zero; one; two; three; four; five; six; seven; eight; nine|]
                
                output |> Array.map (fun x -> Array.findIndex (fun e -> e = sort x) mapping |> string) |> String.Concat |> int
            ) 

        Assert.Equal(1097568, result)