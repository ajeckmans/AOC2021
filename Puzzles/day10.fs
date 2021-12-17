namespace Puzzles

open FSharpPlus
open Puzzles
open FParsec.CharParsers
open FParsec

module Day10 =
    let input = inputs.ReadAllLines "day10.txt"
       
    type Token =
        | EmptyBrackets
        | SquareBrackets of Token list
        | CurlyBrackets of Token list
        | AngleBrackets of Token list
        | RoundBrackets of Token list
   
    let emptyBrackets =
        choice [
            (skipString "()" >>% EmptyBrackets)
            (skipString "[]" >>% EmptyBrackets)
            (skipString "{}" >>% EmptyBrackets)
            (skipString "<>" >>% EmptyBrackets) 
        ]
        
    let token, tokenRef = createParserForwardedToRef<Token, unit>()

    let list s e f = between (skipString s) (skipString e) (many token |>> f)
    
    let squareBrackets = (list "[" "]" SquareBrackets)
    let curlyBrackets =  (list "{" "}" CurlyBrackets)
    let angleBrackets = (list "<" ">" AngleBrackets)
    let roundBrackets =  (list "(" ")" RoundBrackets)
    
    tokenRef.Value <- choice [
        emptyBrackets
        squareBrackets
        curlyBrackets
        angleBrackets
        roundBrackets
    ]
    
    let lineParser = (many token) .>>  eof
        
    let test p str =
        match run p str with
        | Success(result, _, pos)   -> $"Success: %A{result}"
        | Failure(errorMsg, _, _) -> $"Failure: %s{errorMsg}"
    
    let solve_1 (input:seq<string>) =
        input
        |> Seq.map (fun line ->
            match run lineParser line with
            | Success _ -> 0
            | Failure(_, error, _) ->
                if error.Position.Index < line.Length then
                    match line[error.Position.Index |> int] with
                    | ')' -> 3
                    | ']' -> 57
                    | '}' -> 1197
                    | '>' -> 25137
                    | _ -> failwith "oops"
                    
                else 0
        )
        |> Seq.sum
            
    let isClosingChar char = char = ")" || char = "]" || char = "}" || char = ">" 
    
    type Status = | Complete of string list | Incomplete of string list
    
    let rec autocomplete string status =
        match status with
        | Complete characters -> characters
        | Incomplete characters ->
            match run lineParser string with
                | Success _ -> autocomplete string (Complete characters)
                | Failure(_, error, _) ->
                    if error.Position.Index >= string.Length then
                        let possibleClosing = error.Messages |> ErrorMessageList.ToSortedArray |> Array.map (fun x -> (x :?> ErrorMessage.ExpectedString).String)  |> Array.find isClosingChar
                        autocomplete (string + possibleClosing) (Incomplete (characters @ [possibleClosing]))
                    else failwith "syntax error"
    
    let solve_2 input =
        let scores =
            input
            |> Seq.filter (fun line -> run lineParser line
                                     |> fun x ->
                                         match x with
                                         | Success _ -> false
                                         | Failure(_, error,_) -> error.Position.Index >= line.Length
                                         )
            |> Seq.map (fun line ->
                let result = autocomplete line (Incomplete [])
                result |> List.fold (fun state e ->
                    let eScore =
                        match e with
                        | ")" -> 1L
                        | "]" -> 2L
                        | "}" -> 3L
                        | ">" -> 4L
                        | _ -> failwith "oops"
                    (state * 5L) + eScore) 0L
            )
            |> Array.ofSeq
            |> Array.sort
            
        scores[(scores.Length / 2)]