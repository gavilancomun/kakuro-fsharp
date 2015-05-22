module Kakuro.Core

type Cell =
| Empty
| Across of across: int
| Down of down: int
| DownAcross of down:int * across: int


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
