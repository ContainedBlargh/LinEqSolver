module SolverTests

open System
open Xunit
open Solver

[<Fact>]
let ``isSolved returns true on trivial test`` () =
  let test = array2D [[0.0f; 1.0f; 4.0f]; [1.0f; 0.0f; 2.0f]]
  let result = Solver.isSolved test
  Assert.True result

[<Fact>]
let ``ìsSolved returns false on multiple 1-form variables test`` () =
  let test = array2D [[0.0f; 1.0f; 4.0f]; [1.0f; 1.0f; 2.0f]]
  let result = Solver.isSolved test
  Assert.False result

[<Fact>]
let ``isSolved returns false on multiple rows with same variable determined test`` () =
  let test = array2D [[0.0f; 1.0f; 1.0f]; [0.0f; 1.0f; 1.0f]]
  let result = Solver.isSolved test
  Assert.False result

[<Fact>]
let ``ìsSolved returns true on solved 3-variable system test`` () =
  let matrix = array2D [[0.0f; 0.0f; 1.0f; 3.0f]; [1.0f; 0.0f; 0.0f; 1.0f]; [0.0f; 1.0f; 0.0f; 4.0f]]
  let result = Solver.isSolved matrix
  Assert.True result

[<Fact>]
let ``ìsSolved returns false on bogus 3-variable system test`` () =
  let matrix = array2D [[9.0f; 5.0f; 1.0f; 3.0f]; [1.0f; 2.0f; 1.0f; 1.0f]; [16.0f; 1.0f; 8.0f; 4.0f]]
  let result = Solver.isSolved matrix
  Assert.False result

[<Fact>]
let ``isInconsistent returns true on 1.1.49 final matrix`` () =
  let matrix = array2D [[1.0f; 1.0f; -2.0f; 3.0f]; [0.0f; -5.0f; 10.0f; -8.0f]; [0.0f; 0.0f; 0.0f; 10.0f]]
  let result = Solver.isInconsistent matrix
  Assert.True result

//Okay, I'm convinced that the Solver knows when it has solved an equation system.
