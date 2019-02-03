module Matrix

open System

let geo = [|"x"; "y"; "z"|]
let alpha = ['a' .. 'z'] |> List.toArray |> Array.map string

type VarStyle =
  | ZeroIndexed //x_0, x_1, x_2, ...
  | OneIndexed // x_1, x_2, x_3, ...
  | Alphabetical //a, b, c, d, e, f, ...
  | Geometric //x, y, z

(*
Takes a float32[,] and converts it into a human-readable string.
*)
let stringify (m: float32[,]) : string =
  //Imperative solution ahead.
  let mutable output = ""
  for r = 0 to Array2D.length1 m - 1 do
    output <- output + "[ "
    for c = 0 to Array2D.length2 m - 3 do
      output <- output + sprintf "%.2f, " m.[r, c]
    output <- output + sprintf "%.2f " m.[r, Array2D.length2 m - 2]
    output <- output + sprintf "| %.2f ]" m.[r, Array2D.length2 m - 1] + Environment.NewLine
  output

(*
Takes a matrix and prints it as a latex string.
*)
let texify (style: VarStyle) (m: float32[,]) : string =
  let varl = Array2D.length1 m
  let lbl i = 
    match style with
    | Geometric when varl < 3 -> geo.[i]
    | Alphabetical when varl < 26 -> alpha.[i]
    | ZeroIndexed -> sprintf "x_{%d}" i
    | OneIndexed -> sprintf "x_{%d}" (i + 1)
    | _ -> sprintf "x_{%d}" i
  //Imperative solution ahead.
  let l2 = Array2D.length2 m
  let cs = String.replicate (l2 - 1) "c"
  let mutable output = @"\[" + Environment.NewLine + (sprintf @"\begin{array}{@{}%s|c@{}}" cs) + Environment.NewLine
  for r = 0 to Array2D.length1 m - 1 do
    for c = 0 to Array2D.length2 m - 2 do
      output <- output + sprintf "%.0f%s & " m.[r, c] (lbl c)
    output <- output + sprintf @"%.0f%s\\" m.[r, l2 - 1] (lbl (l2 - 1)) + Environment.NewLine
  output + @"\end{array}" + Environment.NewLine + @"\]" + Environment.NewLine


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
let foldCoefficientColumns (func: 'state -> float32[] -> 'state) (state: 'state) (m: float32[,]): 'state =
  let mutable listOfArrays = []
  for c = 0 to Array2D.length2 m - 2 do
    listOfArrays <- ((foldColumn (fun acc i -> i::acc) [] m c) |> List.rev |> List.toArray)::listOfArrays
  listOfArrays
  |> List.rev
  |> List.fold func state

let foldCoefficientRows (func: 'state -> float32[] -> 'state) (state: 'state) (m: float32[,]): 'state =
  let mutable listOfArrays = []
  let l2 = Array2D.length2 m
  for r = 0 to Array2D.length1 m - 1 do
    listOfArrays <- (Array.fold (fun acc e -> e::acc) [] m.[r, 0..(l2 - 1)] |> List.rev |> List.toArray)::listOfArrays
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
