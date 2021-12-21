namespace Puzzles

open System.IO
open System
open Puzzles
open FSharpPlus
open Xunit

module Day13 =
    let input () =
        use stream = inputs.GetResourceStream("day13.txt")
        use reader = new StreamReader(stream)
        
        let dots =
            seq {
                let mutable shouldContinue = true
                while not reader.EndOfStream && shouldContinue do
                    let line =  reader.ReadLine()
                    shouldContinue <- not (String.IsNullOrWhiteSpace(line))
                    if shouldContinue then yield line
            }
            |> Seq.map (sscanf "%i,%i")
            |> List.ofSeq
        
        let instructions =
            seq {
                let mutable shouldContinue = true
                while not reader.EndOfStream && shouldContinue do
                    let line =  reader.ReadLine()
                    shouldContinue <- not (String.IsNullOrWhiteSpace(line))
                    if shouldContinue then yield line
            }
            |> Seq.map (sscanf "fold along %s=%i")
            |> List.ofSeq
            
        (dots, instructions)
           
       
    let foldPaper paper (axes, index) =
         paper |> Seq.map (fun (x,y) ->
                if axes = "x" && x >= index then
                    ((2*index) - x, y)
                else if axes = "y" && y >= index then
                    (x, (2*index) - y)
                else (x,y)
                )
        |> Seq.distinct

   
    [<Fact>]
    let ``part1: actual input`` () =
        let paper, instructions = input()
        
        let result = instructions |> Seq.head |> Seq.singleton |> Seq.fold foldPaper paper |> Seq.length

        Assert.Equal(716, result)

        
    [<Fact>]
    let ``part2: actual input`` () =
        let paper, instructions = input()
        
        let result =
            instructions
            |> Seq.fold foldPaper paper
            |> Seq.sort
            |> List.ofSeq
        
        let xSize, ySize =
            result
            |> Seq.fold ( fun (maxX, maxY) (x,y) -> Math.Max(x , maxX), Math.Max(y , maxY)) (Int32.MinValue, Int32.MinValue)
        
        let writtenPaper = Array2D.create (xSize + 1) (ySize + 1) 0
        result |> Seq.iter (fun (x,y) -> writtenPaper[x,y] <- 1)
        
        use  sw = new StringWriter()
        for y in 0 .. ySize do
            for x in 0 .. xSize do
                if writtenPaper[x,y] = 1 then
                    sw.Write("x")
                else 
                    sw.Write(" ")
            sw.WriteLine(" ")
        
        Assert.Equal(
            "xxx  xxx   xx  x  x xxxx xxx  x    xxx  
x  x x  x x  x x x  x    x  x x    x  x 
x  x x  x x    xx   xxx  xxx  x    x  x 
xxx  xxx  x    x x  x    x  x x    xxx  
x x  x    x  x x x  x    x  x x    x x  
x  x x     xx  x  x x    xxx  xxxx x  x 
"
            , sw.ToString())
