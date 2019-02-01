module Matrix

open System

let stringify (m: float32[,]) : string =
  let mutable output = ""
  for r = 0 to Array2D.length1 m - 1 do
    output <- output + "[ "
    for c = 0 to Array2D.length2 m - 2 do
      output <- output + sprintf "%.2f, " m.[r, c]
    output <- output + sprintf "| %.2f ]" m.[r, Array2D.length2 m - 1] + Environment.NewLine
  output
