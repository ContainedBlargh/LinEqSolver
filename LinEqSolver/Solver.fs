(*
The part of the system where the magic happens.

Needless to say, the magic is not defined yet :P
*)
module Solver

type Step =
  | AddRowsTo of int * int //Add the two rows, save at last param.
  | ScaleRow of int * float32
  | SwapRows of int * int
  | GiveUp

let stepsToString (steps: Step list): string =
  failwith "TODO: implement Solver.stepsToString" 
  //TODO: take the list of steps and convert it into a human readable string.
  //Should be fairly straightforward.

(*
Determine if the matrix is already solved.
*)
let isSolved (matrix: float32[,]): bool =
  failwith "TODO: define the Solver.isSolved function"

(*
The recursive step of the solving function.
Should be private.
*)
let rec solveRec (steps: Step list, matrix: float32[,]): (Step list) * float32[,] =
  failwith "TODO: define the Solver.solveRec function"
  //Using the 3 types of steps, convert the given matrix to the echelon form
  //If there is no solution, give up.

  //This is actually just a conventional dynamic programming problem;
  //The task is to find the minimum required moves to solve the matrix
  //Meaning that we try all combinations.
  //Giving up should also be an option, some systems are unsolveable.
  //Giving up should therefore be a costly move, but a finite move.

let solve (matrix: float32[,]): string = 
  failwith "TODO: implement Solver.solve"