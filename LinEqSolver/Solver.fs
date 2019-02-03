(*
The part of the system where the magic happens.

Needless to say, the magic is not defined yet :P
*)
module Solver

open System
open Matrix

type Step =
  | AddRowTo of int * float32 * int * float32[,] //Add a multiple of one row to another row, save at the latter
  | ScaleRow of int * float32 * float32[,]
  | SwapRows of int * int * float32[,]

let stepToString (styleOpt: VarStyle option) (step: Step) =
  let stringify = 
    match styleOpt with
    | None -> Matrix.stringify
    | Some(style) -> Matrix.texify style
  match step with
  | AddRowTo(src, scale, dest, mat) -> sprintf "Added row %d multiplied by %0.2f to row %d:" src scale dest 
                                          + Environment.NewLine + stringify mat
  | ScaleRow(i, scalar, mat) -> sprintf "Scaled row %d with %.2f:" i scalar + Environment.NewLine + stringify mat
  | SwapRows(i, j, mat) -> sprintf "Swapped row %d with row %d:" i j + Environment.NewLine + stringify mat

(*
Prints the steps taken on a system as a string.
The tex parameter determines whether to print matrices as latex matrices or not.
*)

let stepsToString (styleOpt: VarStyle option) (steps: Step list): string =
  if List.isEmpty steps then
    "0: No steps taken to solve this system yet."
  else
    steps
    |> List.mapi (fun i step -> (i, step))
    |> List.fold (fun acc (i, step) -> acc + sprintf "%d: %s" i (stepToString styleOpt step) + Environment.NewLine ) ""

(*
Determine if the matrix is already solved.
This is simply done by checking if we have reached echelon form.
*)
let isSolved (matrix: float32[,]): bool =
  
  let innerFolder (onePresent: bool, othersZero: bool) (c: float32) =
    match c with
    | 1.0f when onePresent -> (onePresent, false)
    | 1.0f -> (true, othersZero)
    | 0.0f -> (onePresent, othersZero)
    | _ -> (false, false)

  let folder acc (coefRow: float32[]) =
    if not acc then //Short curcuit this stuff, no need to calc twice.
      false
    else
      coefRow
      |> Array.fold innerFolder (false, true)
      |> fun (a, b) -> acc && a && b
  Matrix.foldCoefficientColumns (folder) true matrix

let l1 m = Array2D.length1 m
let l2 m = Array2D.length2 m

let addRowTo s scale d (m: float32[,]): float32[,] * Step =
  let source = m.[s, 0..] |> Array.map (fun x -> x * scale)
  let dest = m.[d, 0..]
  let res = Array.map2 (fun x y -> x + y) source dest

  let initializer r c = 
    if r = d then
      res.[c]
    else
      m.[r, c]
  
  let m' = Array2D.init (l1 m) (l2 m) initializer
  m', AddRowTo(s, scale, d, m')

let scaleRow d scalar (m: float32[,]): float32[,] * Step =
  let initializer r c =
    if r = d then
      m.[r, c] * scalar
    else
      m.[r, c]
  
  let m' =  Array2D.init (l1 m) (l2 m) initializer
  m', ScaleRow(d, scalar, m')

let swapRows i j (m: float32[,]): float32[,] * Step =
  let initializer r c =
    if r = i then
      m.[j, c]
    else if r = j then
      m.[i, c]
    else
      m.[r, c]

  let m' = Array2D.init (l1 m) (l2 m) initializer
  m', SwapRows(i, j, m')

(*
Another, important check.
This is one of the ways illustrated in the book that we can detect inconsistensies.
*)
let isInconsistent (m: float32[,]): bool =
  let rhs = m.[0.., l2 m - 1] |> Array.toList
  let folder (acc: float32 list) coefRow = 
    (Array.sum coefRow)::acc
  let sums = Matrix.foldCoefficientRows folder [] m
  List.fold2 (fun acc sum r -> if sum = 0.0f then acc && r = 0.0f else acc && true) true sums rhs

let check (matrix: float32[,]): string = 
  match matrix with
  | m when (isSolved m) -> "solved:" + Environment.NewLine + Matrix.stringifyResults m
  | m when (isInconsistent m) -> "inconsistent:" + Environment.NewLine + Matrix.stringify m
  | _ -> "not yet solved."
