module Matrix

open System


(*
Takes a float32[,] and converts it into a human-readable string.
*)
let stringify (m: float32[,]) : string =
  //Imperative solution ahead.
  let mutable output = ""
  for r = 0 to Array2D.length1 m - 1 do
    output <- output + "[ "
    for c = 0 to Array2D.length2 m - 2 do
      output <- output + sprintf "%.2f, " m.[r, c]
    output <- output + sprintf "| %.2f ]" m.[r, Array2D.length2 m - 1] + Environment.NewLine
  output

let foldRow (func: 'state -> float32 -> 'state) (state: 'state) (m: float32[,]) (r: int): 'state =
  Array.fold func state m.[r, 0..]

let foldColumn (func: 'state -> float32 -> 'state) (state: 'state) (m: float32[,]) (c: int): 'state =
  Array.fold func state m.[0.., c]

let foldRows (func: 'state -> float32[] -> 'state) (state: 'state) (m: float32[,]): 'state =
  let mutable listOfArrays = []
  for r = 0 to Array2D.length1 m - 1 do
    listOfArrays <- ((foldRow (fun acc i -> i::acc) [] m r) |> List.rev |> List.toArray)::listOfArrays
  listOfArrays
  |> List.rev
  |> List.fold func state

(*
Folds over each of the coefficient columns.
*)
let foldCoefficients (func: 'state -> float32[] -> 'state) (state: 'state) (m: float32[,]): 'state =
  let mutable listOfArrays = []
  for c = 0 to Array2D.length2 m - 2 do
    listOfArrays <- ((foldColumn (fun acc i -> i::acc) [] m c) |> List.rev |> List.toArray)::listOfArrays
  listOfArrays
  |> List.rev
  |> List.fold func state

(*
Fold over the result columns.
*)
let foldResults (func: 'state -> float32 -> 'state) (state: 'state) (m: float32[,]): 'state =
  foldColumn func state m (Array2D.length2 m - 1)

(*
Produce a string representation of the results column.
*)
let stringifyResults (m: float32[,]): string =
  let folder (strAcc: string, i: int) (res: float32): string * int =
    (strAcc + sprintf "\tVariable %d = %0.2f" i res + Environment.NewLine, i + 1)
  foldResults folder ("", 1) m |> fst
