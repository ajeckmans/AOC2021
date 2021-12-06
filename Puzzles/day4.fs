namespace Puzzles

open System.IO
open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day4 =    
    let input =
        use stream = inputs.GetResourceStream("day4.txt")
        use reader = new StreamReader(stream)

        let numbers =
            reader.ReadLine()
            |> fun s -> s.Split(',')
            |> Array.ofSeq

        let cards = 
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine() |> ignore
                    let numbers =  Array2D.create 5 5 ""
                    for i in 0 .. 4 do
                        numbers.[i,*] <-
                            reader.ReadLine()
                            |> String.replace "  " " "
                            |> String.trimWhiteSpaces
                            |> String.split [ " " ]
                            |> Array.ofSeq
                        
                    yield numbers
            } |> List.ofSeq
        
        numbers, cards

    let updateCard number card =
         Array2D.map (fun x ->
             match x with
             | x when x = number -> "x"
             | _ -> x) card
        
    let cardIsWinner (card: string[,]) =
        [0 .. 4]
        |> Seq.exists (fun i ->
            card.[i,*] |> Seq.forall (fun x -> x = "x" )
            || card.[*,i] |> Seq.forall (fun x -> x = "x" )
        )
        
    let calculateCardScore currentNumber card =
        let sumOfUnmarked =
            card
            |> Seq.cast<string>
            |> Seq.where (fun x -> x <> "x" )
            |> Seq.sumBy int
        
        sumOfUnmarked * (int currentNumber)
        
    let solve_1 (input: string[] * string[,] list) =      
        let numbers = input |> fst
        
        seq {
            let mutable cards = input |> snd 
            let mutable cardWon = false
            let mutable step = 0
            
            while cardWon = false do
                let currentNumber = numbers.[step]
                cards <- cards |> List.map (updateCard currentNumber)
                
                match cards |> Seq.tryFind cardIsWinner with
                | Some card ->
                    cardWon <- true
                    yield calculateCardScore currentNumber card 
                | _ -> ()

                step <- step + 1
        } |> Seq.head
        
    let solve_2 (input: string[] * string[,] list) =      
        let numbers = input |> fst
        
        seq {
            let mutable cards = input |> snd 
            let mutable cardWon = false
            let mutable step = 0
            
            while cardWon = false do
                let currentNumber = numbers.[step]
                let newCards, changed =
                    cards
                    |> List.mapFold (fun state elem ->
                        let newCard = updateCard currentNumber elem
                        if newCard <> elem && cardIsWinner elem = false && cardIsWinner newCard then
                            newCard, state @ [newCard]
                        else
                            newCard, state
                        ) []
                cards <- newCards
                    
                if cards |> Seq.forall cardIsWinner then
                    yield changed
                        |> Seq.last
                        |> calculateCardScore currentNumber
                    
                step <- step + 1
        } |> Seq.head