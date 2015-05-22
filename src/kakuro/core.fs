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
  List.iter (fun (x : IDraw) -> printf "%s" <| x.draw()) row
  printfn "\n"

let drawGrid(grid: List<List<IDraw>>) =
  List.iter (fun (row: List<IDraw>) -> drawRow(row)) grid

[<EntryPoint>]
let main argv = 
    let grid1 : List<List<IDraw>> = 
        [[ e(); d 1; a 2; da(3, 4) ]
         [ e(); d 1; a 2; da(3, 4) ]]
    drawGrid grid1
    0 // return an integer exit code
