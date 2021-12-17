namespace Puzzles

open System
open Puzzles
open FSharpPlus
open Xunit

module Day12 =
    let input () =
       inputs.ReadAllLines "day12.txt"
       |> Seq.map (sscanf "%s-%s")
       |> List.ofSeq
       
    let findNext map path =
        let last = path |> List.head
        if last = "end" then
            [path]
        else
            Map.find last map |> List.map (fun x -> [x] @ path)
            
    let rec findPaths map paths isLegal =
        let newPaths = paths
                       |> List.collect (findNext map)
                       |> List.distinct
                       |> List.filter isLegal
        if newPaths.Length <> paths.Length then
            findPaths map newPaths isLegal
        else
            paths
    
    [<Fact>]
    let ``part1: actual input`` () =
        let isLegal paths =
            paths
            |> List.groupBy id
            |> List.forall (fun (x, list) -> if Char.IsUpper (x |> String.item 0) then true else list |> length = 1  )
        
        let map = input ()
                  |> List.collect (fun (a, b) -> [a,b;b,a])
                  |> List.groupBy fst |> List.map (fun (k, v) -> k, v |> List.map snd ) |> Map.ofList
        
        let result =
            findPaths map [["start"]] isLegal
            |> List.filter (fun path -> path |> List.head = "end")
            |> List.length
        
        Assert.Equal(3576, result)        

    [<Fact>]
    let ``part2: actual input`` () =
        let isLegal paths =
            let nodeVisitCount = paths |> List.groupBy id |> List.map (fun (k, list) -> k, length list)
            let startCount = nodeVisitCount |> List.find (fun (k, _) -> k = "start") |> snd
            
            if (startCount <> 1) then
                false
            else 
                let smallCaves = nodeVisitCount |> List.filter (fun (k, _) -> Char.IsLower (k |> String.item 0))            
                let smallCavesVisitedTwice = smallCaves |> List.filter (fun (k, list) -> list = 2)
                let smallCavesVisitedMoreThanTwice = smallCaves |> List.filter (fun (k, list) -> list > 2) 
                
                (smallCavesVisitedTwice.Length <= 1) && (List.isEmpty smallCavesVisitedMoreThanTwice)
        
        let map = input ()
                  |> List.collect (fun (a, b) -> [a,b;b,a])
                  |> List.groupBy fst |> List.map (fun (k, v) -> k, v |> List.map snd ) |> Map.ofList
      
      
        let result =
            findPaths map [["start"]] isLegal
            |> List.filter (fun path -> path |> List.head = "end")
            |> List.length
            
        Assert.Equal(84271, result)
