namespace Puzzles

open System.IO
open FSharpPlus
open Puzzles
open Xunit

module Day4 =    
    let input =
        use stream = inputs.GetResourceStream("day4.txt")
        use reader = new StreamReader(stream)

        let numbers =
            reader.ReadLine()
            |> fun s -> s.Split(',')
            |> List.ofSeq

        let cards = 
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine() |> ignore
                    let numbers =  Array2D.create 5 5 ""
                    for i in 0 .. 4 do
                        numbers[i,*] <-
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
            card[i,*] |> Seq.forall (fun x -> x = "x" )
            || card[*,i] |> Seq.forall (fun x -> x = "x" )
        )
        
    let calculateCardScore currentNumber card =
        let sumOfUnmarked =
            card
            |> Seq.cast<string>
            |> Seq.where (fun x -> x <> "x" )
            |> Seq.sumBy (int)
        
        sumOfUnmarked * (currentNumber |> int)
        
    let solve_1 (input: string list * string[,] list) =
        let rec getWinningCard (numbers, cards) =
            match numbers with
            | [] -> None
            | currentNumber::remainders ->  
                let newCards = cards |> List.map (updateCard currentNumber)
                match newCards |> Seq.tryFind cardIsWinner with
                    | Some card -> Some (calculateCardScore currentNumber card) 
                    | _ -> getWinningCard (remainders, newCards)
        getWinningCard input
        
    let solve_2 (input: string list * string[,] list) =
        let rec getWinningCard (numbers, cards) =
            match numbers with
            | [] -> None
            | currentNumber::remainders ->
                let newCards, changed =
                    cards
                    |> List.mapFold (fun state elem ->
                        let newCard = updateCard currentNumber elem
                        if cardIsWinner elem = false && cardIsWinner newCard then
                            newCard, state @ [newCard]
                        else
                            newCard, state
                        ) []
                if newCards |> Seq.forall cardIsWinner then
                    changed
                    |> Seq.head
                    |> fun card -> Some (calculateCardScore currentNumber card)
                else  getWinningCard (remainders, newCards)
                 
        getWinningCard input
        
        
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(Some 51034, result)
        
        
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(Some 5434, result)