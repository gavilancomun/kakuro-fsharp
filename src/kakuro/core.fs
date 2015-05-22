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
let e() = Empty()
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

let allDifferent (nums : List<int>) = nums.Length = (set nums).Count

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

[<EntryPoint>]
let main argv = 
    let grid1 : List<List<IDraw>> = 
        [ [ e()
            d 1
            a 2
            da (3, 4)
            v ]
          [ v
            e()
            d 1
            a 2
            da (3, 4) ] ]
    printf "%s" <| drawGrid grid1
    0 // return an integer exit code
