module MatrixTests

open System
open Xunit
open Solver
open Matrix
open System.Diagnostics

[<Fact>]
let ``stringifyResults returns correct string`` () =
  let test = array2D [[0.0f; 1.0f; 4.0f]; [1.0f; 0.0f; 2.0f]]
  let result = Matrix.stringifyResults test
  Assert.Contains("1 = 4.0", result);
  Assert.Contains("2 = 2.0", result);
