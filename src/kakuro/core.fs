module Kakuro.Core

type IDraw = 
    abstract draw : unit -> string

type Empty() = 
    interface IDraw with
        member __.draw() = "   -----  "

type Down(down : int) = 
    interface IDraw with
        member __.draw() = sprintf "   %2d\\--  " down

type Across(across : int) = 
    interface IDraw with
        member __.draw() = sprintf "   --\\%2d  " across

type DownAcross(down : int, across : int) = 
    interface IDraw with
        member __.draw() = sprintf "   %2d\\%2d  " down across

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
