module Command

open System.Text.RegularExpressions
open System

type Command = 
  | Define of string * float32[,]
  | Solve of string
  | Exit

let (|Regex|_|) pattern input =
  let regex = new Regex(pattern, RegexOptions.Singleline)
  let m = regex.Match(input)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

(*
The function that parses matrices.
What should a matrix look like? What is easiest to write?
def a
[ 1, 0, 0 | 1 ]
[ 0, 1, 0 | 2 ]
[ 0, 0, 1 | 3 ]
;
The above could be a valid command that makes an already solved matrix.
*)
let parseMatrix (mat: string): float32[,] =

  let parseLine line =
    let m = Regex.Match (line, @"\[ ?(.*) ?\| ?(\d) ?\]")
    let coefficients = m.Groups.Item 1 
                       |> (fun g -> g.Value.Split(","))
                       |> Array.map (fun s -> s.Trim() |> float32)
    let constant = m.Groups.Item 2 |> (fun g -> g.Value) |> float32
    Array.append coefficients [|constant|]
  
  let combineArrays outerArray array =
    Array.append outerArray [|array|]

  mat.TrimStart().Split(Environment.NewLine)
  |> Array.toList
  |> (fun l -> List.truncate (List.length l - 1) l)
  |> List.map parseLine
  |> List.fold combineArrays [||]
  |> array2D
  
let parseCommand (input: string) : Command option = 
  let lower = input.ToLower()
  match (lower) with
  | Regex @"def (\w*) ?(\r|\n|.*);" [name; matrix] -> Some(Define(name, (parseMatrix matrix)))
  | Regex @"solve (\w*);" [name] -> Some(Solve(name))
  | Regex @"(?:exit|quit);" [] -> Some(Exit)
  | _ -> None
