namespace Puzzles

open FSharpPlus.Control
open Puzzles
open Xunit

module Day11 =
    let input =
       inputs.readAllLines "day11.txt"
       |> Seq.map (fun x -> x :> seq<char>)
        
        
    let indexesOf f (array: 'a[,]) =
        seq {
            for x = 0 to (array.GetLength 0) - 1 do
                for y = 0 to (array.GetLength 1) - 1 do
                    if f array[x,y] then yield (x,y)
        } |> List.ofSeq
        
    let getSurrounding x y x_max y_max =
        [| for x' = x-1 to x+1 do
             for y' = y-1 to y+1 do
                 x', y'
        |]|> Array.filter (fun (x,y) -> x > -1 && x < x_max + 1 && y > -1 && y < y_max + 1  )
    
    let flashField field neighbourMap =
        let rec flash field toFlash flashed =
            match toFlash with
            | [] -> field, flashed
            | flashing::yetToFlash ->
                let neighbours = neighbourMap |> Map.find flashing
                let newField = neighbours  |> Array.fold (fun (state:int[,]) (xn, xy) -> state[xn,xy] <- state[xn,xy]+1;state ) field
                let increasedNeighbours = neighbours |> List.ofArray |> List.filter (fun (x,y) -> field[x,y] = 10)
                flash newField (yetToFlash @ increasedNeighbours |> List.except flashed) (flashed @ [flashing])
                
        flash field (field |> indexesOf ((=) 10)) []
        
    let resetFlashed field = field |> Array2D.map (fun x -> if x > 9 then 0 else x)
        
    [<Fact>]
    let ``part1: actual input`` () =
        let initial = array2D input |> Array2D.map (string >> int)

        let x_max = (initial.GetLength 0) - 1
        let y_max = (initial.GetLength 1) - 1
        
        let neighbourMap =
            [0..x_max]
            |> Seq.collect (fun x -> [0..y_max] |> Seq.map (fun y -> x,y))
            |> Seq.map (fun (x, y) -> (x,y), (getSurrounding x y x_max y_max))
            |> Map.ofSeq
            
        let result =
            [1 .. 100]
            |> List.fold (fun (state, flashedCount) _ ->
                let increased = state |> Array2D.map (fun x -> x + 1)
                let newField, flashed = flashField increased neighbourMap
                (newField |> resetFlashed, flashedCount + flashed.Length)
            ) (initial, 0) |> snd
    
        Assert.Equal(1625, result)

        
    [<Fact>]
    let ``part2: actual input`` () =
        let initial = array2D input |> Array2D.map (string >> int)

        let x_max = (initial.GetLength 0) - 1
        let y_max = (initial.GetLength 1) - 1
        
        let neighbourMap =
            [0..x_max]
            |> Seq.collect (fun x -> [0..y_max] |> Seq.map (fun y -> x,y))
            |> Seq.map (fun (x, y) -> (x,y), (getSurrounding x y x_max y_max))
            |> Map.ofSeq
        
        let rec findSynchronized state step =
            let increased = state |> Array2D.map (fun x -> x + 1)
            let flashedField, _ = flashField increased neighbourMap
            let newField = flashedField |> resetFlashed
            if newField |> Seq.cast<int> |> Seq.forall ((=) 0) then
                step + 1
            else
                findSynchronized newField  (step + 1)
        
        let result = findSynchronized initial 0
        
        Assert.Equal(244, result)
