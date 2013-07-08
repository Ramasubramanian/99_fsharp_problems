module nqueens

type Position = Position of int * int

let abs x = if x < 0 then x * -1 else x

let canPlace x y = function 
    Position(i,j) ->
            x <> i && y <> j && (abs (x - i) <> abs(y - j))

let cannotPlace x y p = canPlace x y p |> not

let last list = 
    List.nth list list.Length

let nextPos row col n queens = 
    match col with
    | 1 -> Some(Position(row+1, col))
    | col when row >= n -> None
    | col -> 
        let possibles = [for i in (row+1)..n -> Position(i,col)]
        let matches x y = List.fold (fun acc elem -> acc && canPlace x y elem) true queens
        let result = List.filter (fun (Position(row,col)) -> matches row col) possibles 
        if result.Length = 0 then None else Some(result.Head)

let printRow col n =
    let list = [for i in 1 .. n do
                match i with
                | i when col = i -> yield 'Q'
                | i -> yield '-' ]
    List.iter (fun e -> printf "\t%A" e) list 
    printfn ""    

let printOutput list =
    let n = List.length list
    list
    |> List.sortBy (fun (Position(row,col)) -> row)
    |> List.rev
    |> List.iter (fun (Position(_,col)) -> printRow col n)
    printfn ""

/// Solve using backtracking algorithm implemented with recursion
let solveNQueens n =
    let rec solve row col n queens =
        match col with 
        | col when col > n -> queens
        | col ->
            let possible = nextPos row col n queens
            match possible with 
            | None -> if row = n then [] else solve (row+1) col n queens
            | Some(Position(x,y)) -> 
                let result = solve 0 (col+1) n (queens @ [possible.Value])
                if result.Length = 0 then solve (row+1) col n queens else result
    solve 0 1 n [] |> printOutput 


/// Alternative solution - no backtracking just generate and filter out invalid positions
/// finds all the solutions
let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    returnValue

let isValid x y solution = 
    List.exists (fun p -> cannotPlace x y p  ) solution |> not

let nextColPositions col n solution =
    [for i in 1..n -> Position(i,col)]
        |> List.filter (fun (Position(x,y)) -> isValid x y solution) 

let nqueens n = 
    let rec solve col n solutions =
        if col > n then 
            solutions
        else 
           List.collect (fun solution -> [for p in (nextColPositions col n solution) -> solution @ [p]]) solutions
           |> solve (col+1) n 
    let result = solve 1 n [[]]
    printfn "No. of solutions: %A for n=%A" (List.length result) n

///List.map (fun i -> duration (fun() -> nqueens i) ) [1..n] 