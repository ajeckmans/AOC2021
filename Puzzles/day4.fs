namespace Puzzles

open System.IO
open FSharpPlus
open FSharpPlus.Data
open Puzzles

module Day4 =    
    type Number =
        | Called of int
        | Uncalled of int

    type Card = { Numbers: Number[,] }
        
    let input =
        use stream = inputs.GetResourceStream("day4.txt")
        use reader = new StreamReader(stream)

        let numbers =
            reader.ReadLine()
            |> fun s -> s.Split(',')
            |> Seq.map int
            |> Array.ofSeq

        let cards = 
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine() |> ignore
                    let numbers =  Array2D.create 5 5 (Uncalled 0)
                    for i in 0 .. 4 do
                        let e1, e2, e3, e4, e5 =
                            reader.ReadLine()
                            |> String.replace "  " " "
                            |> String.trimWhiteSpaces
                            |> sscanf "%i %i %i %i %i"
                        numbers.[i,*] <- [| Uncalled e1; Uncalled e2; Uncalled e3; Uncalled e4; Uncalled e5|]
                        
                    yield { Numbers = numbers }
            } |> List.ofSeq
        
        numbers, cards

    let updateCard number card =
        { card with Numbers = Array2D.map (fun x ->
                                 match x with
                                 | Uncalled x when x = number -> Called x
                                 | Uncalled x -> Uncalled x
                                 | Called x -> Called x ) card.Numbers
        }
        
    let cardIsWinner card =
        [0 .. 4]
        |> Seq.exists (fun i ->
            card.Numbers.[i,*] |> Seq.forall (fun x -> match x with | Called _ -> true | _ -> false )
            || card.Numbers.[*,i] |> Seq.forall (fun x -> match x with | Called _ -> true | _ -> false )
        )
        
    let calculateCardScore currentNumber card =
        let sumOfUnmarked =
            card.Numbers
            |> Seq.cast<Number>
            |> Seq.where (fun x -> match x with | Uncalled _ -> true | _ -> false )
            |> Seq.sumBy (fun x -> match x with | Uncalled x -> x | _ -> 0 )
        
        sumOfUnmarked * currentNumber
        
    let solve_1 =      
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
        
    let solve_2 =      
        let numbers = input |> fst
        
        seq {
            let mutable cards = input |> snd 
            let mutable cardWon = false
            let mutable step = 0
            
            while cardWon = false do
                let currentNumber = numbers.[step]
                let newCards, changed =
                    cards
                    |> List.mapFold (fun (state: Card list) elem ->
                        let newCard = updateCard currentNumber elem
                        if newCard.Numbers <> elem.Numbers && cardIsWinner elem = false && cardIsWinner newCard then
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