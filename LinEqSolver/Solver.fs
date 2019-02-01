(*
The part of the system where the magic happens.

Needless to say, the magic is not defined yet :P
*)
module Solver

open System

type Step =
  | AddRowTo of int * float32 * int //Add a multiple of one row to another row, save at the latter
  | ScaleRow of int * float32
  | SwapRows of int * int
  | GiveUp

let stepsToString (steps: Step list): string =
  failwith "TODO: implement Solver.stepsToString" 
  //TODO: take the list of steps and convert it into a human readable string.
  //Should be fairly straightforward.
  let stepToString (step: Step): string =
    match step with
    | AddRowTo(src, scale, dest) -> sprintf "Added row %d multiplied by %0.2f to row %d." src scale dest
    | ScaleRow(i, scalar) -> sprintf "Scaled row %d with %.2f." i scalar
    | SwapRows(i, j) -> sprintf "Swapped row %d with row %d." i j
    | GiveUp -> "No solution could be found. Sorry."
  
  if List.isEmpty steps then
    "0: The system is already solved."
  else
    steps
    |> List.mapi (fun i step -> (i, step))
    |> List.fold (fun acc (i, step) -> acc + sprintf "%d: %s" i (stepToString step) + Environment.NewLine ) ""

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
  Matrix.foldCoefficients (folder) true matrix

let l1 m = Array2D.length1 m
let l2 m = Array2D.length2 m

let addRowTo s scale d (m: float32[,]): float32[,] =
  let source = m.[s, 0..] |> Array.map (fun x -> x * scale)
  let dest = m.[d, 0..]
  let res = Array.map2 (fun x y -> x + y) source dest

  let initializer r c = 
    if r = d then
      res.[c]
    else
      m.[r, c]

  Array2D.init (l1 m) (l2 m) initializer 

let scaleRow d scalar (m: float32[,]): float32[,] =
  let initializer r c =
    if r = d then
      m.[r, c] * scalar
    else
      m.[r, c]
  
  Array2D.init (l1 m) (l2 m) initializer

let swapRows i j (m: float32[,]): float32[,] =
  let initializer r c =
    if r = i then
      m.[j, c]
    else if r = j then
      m.[i, c]
    else
      m.[r, c]

  Array2D.init (l1 m) (l2 m) initializer

(*
Another, important check.
This is one of the ways illustrated in the book that we can detect inconsistensies.
*)
let isInconsistent (m: float32[,]): bool =
  let rhs = m.[l2 m - 1, 0..] |> Array.toList
  let folder (acc: float32 list) coefRow = 
    (Array.sum coefRow)::acc
  let sums = Matrix.foldCoefficients folder [] m
  List.fold2 (fun acc sum r -> if sum = 0.0f then acc && r = 0.0f else acc && true) true sums rhs

(*
The recursive step of the solving function.
Should be private.
*)
let rec solveRec (steps: Step list) (matrix: float32[,]): (Step list) * float32[,] =
  if isSolved matrix then
    (List.rev steps, matrix)
  else
    if isInconsistent matrix then
      (List.rev (GiveUp::steps), matrix)      
    else
      failwith "TODO: Implement the rest."
  //Using the 3 types of steps, convert the given matrix to the echelon form
  //If there is no solution, give up.
  
  //This is actually just a conventional dynamic programming problem;
  //The task is to find the minimum required moves to solve the matrix
  //Meaning that we try all combinations.
  //Giving up should also be an option, some systems are unsolveable.
  //Giving up should therefore be a costly move, but a finite move.

let solve (matrix: float32[,]): float32[,] * string = 
  solveRec [] matrix
  |> fun (steps, matrix) -> (matrix, stepsToString steps)
