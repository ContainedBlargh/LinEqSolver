module Command

open System.Text.RegularExpressions
open System
open Solver

type Command = 
  | Define of string * float32[,]
  | Check of string
  | AddRowTo of string * int * float32 * int
  | ScaleRow of string * int * float32
  | SwapRows of string * int * int
  | Trace of string
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
  | Regex @"check (\w*);" [name] -> Some(Check(name))
  | Regex @"trace (\w*);" [name] -> Some(Trace(name))
  | Regex @"(\w*)\.swap (\d+) (\d+);" [name; r1; r2] -> Some(SwapRows(name, r1 |> int, r2 |> int))
  | Regex @"(\w*)\.add (\d+) (\d+);" [name; r1; r2] -> Some(AddRowTo(name, r1 |> int, 1.0f, r2 |> int))
  | Regex @"(\w*)\.add (\d+) ([-+]?[0-9]*\.[0-9]+|[0-9]+) (\d+);" [name; r1; s; r2] -> Some(AddRowTo(name, r1 |> int, s |> float32, r2 |> int))
  | Regex @"(\w*)\.scale (\d+) ([-+]?[0-9]*\.[0-9]+|[0-9]+);" [name; r1; s] -> Some(ScaleRow(name, r1 |> int, s |> float32))
  | Regex @"(?:exit|quit);" [] -> Some(Exit)
  | _ -> None
