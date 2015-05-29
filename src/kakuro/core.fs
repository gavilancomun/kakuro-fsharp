module Kakuro.Core

type Cell = 
  | Empty
  | Down of down: int
  | Across of across: int
  | DownAcross of down: int * across: int
  | Value of values: Set<int>

let draw cell = 
  match cell with
  | Empty -> "   -----  "
  | Down n -> sprintf "   %2d\\--  " n
  | Across n -> sprintf "   --\\%2d  " n
  | DownAcross (down, across) -> sprintf "   %2d\\%2d  " down across
  | Value values -> 
      if 1 = values.Count then 
        values
        |> Set.map (fun x -> "     " + x.ToString() + "    ")
        |> String.concat ""
      else 
         " " +
         ([ 1..9 ]
         |> List.map (fun x -> if Set.contains x values then x.ToString() else ".")
         |> String.concat "")

let a across = Across(across)
let d down = Down(down)
let da down across = DownAcross(down, across)
let e = Empty
let v = Value(set [ 1..9 ])

let drawRow row = 
    (List.map draw row |> String.concat "") + "\n"

let drawGrid grid = 
    "\n" + (List.map drawRow grid |> String.concat "")

let allDifferent (nums : int list) = (nums.Length = (set nums).Count)

let rec permute (vs : Cell list) target (soFar: int list) = 
    if target >= 1 then 
        if soFar.Length = (vs.Length - 1) then [ soFar @ [ target ] ]
        else 
            match vs.[soFar.Length] with
            | Value values -> values
                              |> Seq.collect (fun v -> permute vs (target - v) (soFar @ [ v ]))
                              |> List.ofSeq
            | _ -> []
    else []

let permuteAll vs total = permute vs total []

let isPossible cell n =
    match cell with
    | Value values -> Set.contains n values
    | _ -> false

let rec transpose matrix = 
    match matrix with
    | row :: rows ->
        match row with
        | col :: cols ->
            // Take first elements from all rows of the matrix
            let first = List.map List.head matrix
            // Take remaining elements from all rows of the matrix
            // and then transpose the resulting matrix
            let rest = matrix |> List.map List.tail |> transpose
            first :: rest
        | _ -> []
    | _ -> []

let solveStep (cells : Cell list) total = 
    let final = cells.Length - 1
    permuteAll cells total
    |> List.filter (fun p -> isPossible cells.[final] p.[final])
    |> List.filter allDifferent
    |> transpose
    |> List.map (fun p -> Value(set p))

let rowTarget cell = 
    match cell with
    | Across n -> n
    | DownAcross (d, a) -> a
    | _ -> 0

let colTarget cell =
    match cell with
    | Down d -> d
    | DownAcross (d, a) -> d
    | _ -> 0

let solvePair f pair = 
    match pair with
    | [ nvs; vs ] -> nvs @ (solveStep vs (nvs |> Seq.last |> f))
    | [nvs] -> nvs
    | _ -> []

let solvePairRow = solvePair rowTarget

let solvePairCol = solvePair colTarget

let rec drop n coll =
  match coll with
  | [] -> []
  | x :: xs when (n <= 1) -> xs
  | x :: xs -> drop (n - 1) xs

let rec partitionBy f coll = 
    match coll with
    | [] -> []
    | x :: xs -> 
        let fx = f x
        let run = Seq.takeWhile (fun y -> fx = f y) coll |> Seq.toList
        run :: partitionBy f (drop run.Length coll)

let rec partitionAll n step coll = 
    match coll with
    | [] -> []
    | x :: xs -> 
        let seg = Seq.truncate n coll |> Seq.toList
        seg :: partitionAll n step (drop step coll)

let partitionN n coll = partitionAll n n coll

let isValue cell = 
  match cell with
  | Value vs -> true
  | _ -> false

let solveLine f cells =
    partitionN 2 <| partitionBy isValue cells |> List.collect f

let solveRow = solveLine solvePairRow

let solveCol = solveLine solvePairCol

let solveGrid grid = 
    grid
    |> List.map solveRow
    |> transpose
    |> List.map solveCol
    |> transpose

let grid1 : Cell list list = 
    [ [ e; (d 4); (d 22); e; (d 16); (d 3) ]
      [ (a 3)
        v
        v
        (da 16 6)
        v
        v ]
      [ (a 18)
        v
        v
        v
        v
        v ]
      [ e
        (da 17 23)
        v
        v
        v
        (d 14) ]
      [ (a 9)
        v
        v
        (a 6)
        v
        v ]
      [ (a 15)
        v
        v
        (a 12)
        v
        v ] ]

let rec solver grid =
  let g = solveGrid grid
  if g = grid then
    grid
  else
    drawGrid g |> printf "%s"
    solver g

[<EntryPoint>]
let main _ = 
    let result = solver grid1
    0 // return an integer exit code
