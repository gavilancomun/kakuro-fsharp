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

let a (across : int) = Across(across)
let d (down : int) = Down(down)
let da (down : int, across : int) = DownAcross(down, across)
let e() = Empty()

let drawRow(row: List<IDraw>) = 
  (row
  |> List.map (fun x -> x.draw())
  |> String.concat "") + "\n"

let drawGrid(grid: List<List<IDraw>>) =
  grid
  |> List.map (fun row -> drawRow(row))
  |> String.concat ""

[<EntryPoint>]
let main argv = 
    let grid1 : List<List<IDraw>> = 
        [[ e(); d 1; a 2; da(3, 4) ]
         [ e(); d 1; a 2; da(3, 4) ]]
    printf "%s" <| drawGrid grid1
    0 // return an integer exit code
