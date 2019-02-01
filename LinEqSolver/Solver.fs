module Solver

type Step =
  | AddRowsTo of int * int //Add the two rows, save at last param.
  | ScaleRow of int * float32
  | SwapRows of int * int

let stepsToString (steps: Step list): string =
  ""

let rec solveRec (steps: Step list): Step list =
  []

let solve (matrix: float32[,]): string = 
  ""