module Kakuro.Core

type IDraw = 
    abstract draw : unit -> string

type Empty() = 
    interface IDraw with
        member __.draw() = "   -----  "

type Down(down : int) = 
    interface IDraw with
        member __.draw() = sprintf "   %2d\\--  " down
    
    member __.down = down

type Across(across : int) = 
    interface IDraw with
        member __.draw() = sprintf "   --\\%2d  " across
    
    member __.across = across

type DownAcross(down : int, across : int) = 
    interface IDraw with
        member __.draw() = sprintf "   %2d\\%2d  " down across
    
    member __.down = down
    member __.across = across

type Value(values : Set<int>) = 
    interface IDraw with
        member __.draw() = 
            if 1 = values.Count then 
                values
                |> Set.map (fun x -> "    " + x.ToString() + "    ")
                |> String.concat ""
            else 
                " " + ([ 1..9 ]
                       |> List.map (fun x -> 
                              if Set.contains x values then x.ToString()
                              else ".")
                       |> String.concat "")
    
    member __.values = values

let a (across : int) = Across(across)
let d (down : int) = Down(down)
let da (down : int, across : int) = DownAcross(down, across)
let e = Empty()
let v = Value(set [ 1..9 ])

let drawRow (row : List<IDraw>) = 
    (row
     |> List.map (fun x -> x.draw())
     |> String.concat "")
    + "\n"

let drawGrid (grid : List<List<IDraw>>) = 
    grid
    |> List.map (fun row -> drawRow (row))
    |> String.concat ""

let allDifferent (nums : List<int>) = (nums.Length = (set nums).Count)

let rec permute (vs : List<Value>, target : int, soFar : List<int>) = 
    if target >= 1 then 
        if soFar.Length = (vs.Length - 1) then [ soFar @ [ target ] ]
        else 
            (List.nth vs soFar.Length).values
            |> Seq.collect (fun v -> permute (vs, (target - v), (soFar @ [ v ])))
            |> List.ofSeq
    else []

let permuteAll (vs : List<Value>, total : int) = permute (vs, total, [])
let isPossible (cell : Value, n : int) = Set.contains n cell.values

let rec transpose matrix = 
    match matrix with // matrix is a list<list<int>>
    | row :: rows -> // case when the list of rows is non-empty
        match row with // rows is a list<int>
        | col :: cols -> // case when the row is non-empty
            // Take first elements from all rows of the matrix
            let first = List.map List.head matrix
            // Take remaining elements from all rows of the matrix
            // and then transpose the resulting matrix
            let rest = transpose (List.map List.tail matrix)
            first :: rest
        | _ -> []
    | _ -> []

let solveStep (cells : List<Value>, total : int) = 
    let final = cells.Length - 1
    
    let perms = 
        permuteAll (cells, total)
        |> List.filter (fun p -> isPossible ((List.nth cells final), (List.nth p final)))
        |> List.filter allDifferent
    perms
    |> transpose
    |> List.map (fun p -> Value(set p))

let solvePairRow pair = 
    match pair with
    | [nvs] -> nvs
    | [ nvs; [] ] -> nvs
    | [ nvs : List<IDraw>; vs ] -> 
        nvs @ (solveStep (vs |> List.map (fun x -> x :?> Value), 
                   match Seq.last nvs with
                   | :? Across as x -> x.across
                   | :? DownAcross as x -> x.across
                   | _ -> 0) |> List.map (fun x -> x :> IDraw))
    | _ -> []

let solvePairCol pair = 
    match pair with
    | [nvs] -> nvs
    | [ nvs; [] ] -> nvs
    | [ nvs : List<IDraw>; vs ] -> 
        nvs @ (solveStep (vs |> List.map (fun x -> x :?> Value), 
                   match Seq.last nvs with
                   | :? Down as x -> x.down
                   | :? DownAcross as x -> x.down
                   | _ -> 0) |> List.map (fun x -> x :> IDraw))
    | _ -> []

let rec partitionBy (f, coll) = 
    match coll with
    | [] -> []
    | x :: xs -> 
        let fx = f x
        let run = 
            coll
            |> Seq.takeWhile (fun y -> fx = f y)
            |> Seq.toList
        run :: partitionBy (f, 
                            coll
                            |> Seq.skip run.Length
                            |> Seq.toList)

let rec drop(n, coll) =
  match coll with
  | [] -> []
  | x::xs when (n <= 1) -> xs
  | x::xs -> drop((n - 1), xs)

let rec partitionAll (n, step, coll) = 
    match coll with
    | [] -> []
    | x :: xs -> 
            let seg = coll
                      |> Seq.truncate n
                      |> Seq.toList
            seg :: partitionAll (n, step, 
                                 coll
                                 |> (fun coll -> drop(step, coll))
                                 |> Seq.toList)

let partitionN (n, coll) = partitionAll (n, n, coll)
let solveRow row = 
    partitionN (2, partitionBy ((fun (x : IDraw) -> x :? Value), row)) |> List.collect (fun p -> solvePairRow p)
let solveCol col = 
    partitionN (2, partitionBy ((fun (x : IDraw) -> x :? Value), col)) |> List.collect (fun p -> solvePairCol p)

let peekRow(row) =
    printf "%s" <| drawRow row
    row

let solveGrid (grid : List<List<IDraw>>) = 
    grid
    |> List.map solveRow
    |> List.map peekRow
    |> transpose
    |> List.map solveCol
    |> transpose
    |> List.map peekRow

let grid1 : List<List<IDraw>> = 
    [ [ e
        (d 4)
        (d 22)
        e
        (d 16)
        (d 3) ]
      [ (a 3)
        v
        v
        da (16, 6)
        v
        v ]
      [ (a 18)
        v
        v
        v
        v
        v ]
      [ e
        da (17, 23)
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

[<EntryPoint>]
let main argv = 
    grid1 
    |> solveGrid
    |> drawGrid
    |> printf "%s"

    0 // return an integer exit code
