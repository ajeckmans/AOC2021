namespace Puzzles

open FSharpPlus
open Puzzles
open Xunit

module Day9 =
    let input =
       inputs.ReadAllLines "day9.txt"
       |> Seq.map seq<char>

    let left (x,y) = (x-1, y)
    let right (x,y) = (x+1, y)
    let up (x,y) = (x, y-1)
    let down (x,y) = (x, y+1)
           
    let getNeighbours point max_x max_y =
        let x, y = point
        if x = max_x && y = max_y then [left point; up point]
        else if y = max_y && x = 0 then [ right point; up point]
        else if y = max_y then [ left point; right point; up point]
        else if x = max_x && y = 0 then [ left point; down point]
        else if x = max_x then [ left point; down point; up point]
        else if x = 0 && y = 0 then [right point; down point]
        else if x = 0 then [right point; down point; up point]
        else if y = 0 then [left point; right point; down point]
        else [left point; right point; down point; up point]
        
    let solve_1 (input: seq<seq<char>>) =
       let maxY = (input |> length) - 1
       let maxX = (input |> head |> length) - 1

       [0..maxX] |> Seq.collect (fun x -> [0..maxY] |>Seq.map (fun y -> x, y))
       |> Seq.map (fun (x,y) -> x, y, (input |> Seq.item y |> Seq.item x), getNeighbours (x,y) maxX maxY)
       |> Seq.filter (fun (x, y, pValue, neighbours) ->
           neighbours
           |> Seq.map (fun (nx, ny) -> input |> Seq.item ny |> Seq.item nx )
           |> Seq.forall (fun n -> n > pValue) )
       |> Array.ofSeq
       |> Array.sumBy (fun (_,_,value,_) -> (value |> string |> int) + 1)
       
        
    let getAdjacentBasinPoints point max_x max_y =
       getNeighbours point max_x max_y
       |> Seq.map (fun (nx, ny) -> (nx, ny), input |> Seq.item ny |> Seq.item nx |> string |> int)
       |> Seq.filter (fun (_, value) -> value <> 9)
       |> List.ofSeq

    let rec findBasin list max_x max_y state  =
        match list with
        | [] -> state
        | head::tail ->
            let found = (getAdjacentBasinPoints head max_x max_y |> List.map fst)
            let newList = tail @ (found  |> List.distinct |> List.except state) 
            (findBasin newList max_x max_y (state @ found @ [head] |> List.distinct))
            
    let solve_2 (input: seq<seq<char>>) =
       let maxY = (input |> Seq.length) - 1
       let maxX = (input |> Seq.head |> length) - 1

       [0..maxX]
       |> Seq.collect (fun x -> [0..maxY] |>Seq.map (fun y -> x, y))
       |> Seq.map (fun (x,y) -> x, y, (input |> Seq.item y |> Seq.item x), getNeighbours (x,y) maxX maxY)
       |> Seq.filter (fun (x, y, pValue, neighbours) ->
           neighbours
           |> Seq.map (fun (nx, ny) -> input |> Seq.item ny |> Seq.item nx )
           |> Seq.forall (fun n -> n > pValue) )
       |> Seq.map (fun (x, y, pValue, neighbours)  ->
           let basin = findBasin [(x, y)] maxX maxY []
           ( basin |> List.distinct |> length))
       |> Seq.sortDescending
       |> Seq.take 3
       |> Seq.reduce (*)

   
    [<Fact>]
    let ``part1: actual input`` () =
        let result = solve_1 input
        Assert.Equal(588, result)
       
   
    [<Fact>]
    let ``part2: actual input`` () =
        let result = solve_2 input
        Assert.Equal(964712, result)
       