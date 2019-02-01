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
