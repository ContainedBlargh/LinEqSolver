(* 
  This is a program meant to solve linear equation systems.
  I'm writing this program in order to reinforce my understanding of linear equation systems.
  This should come with the side benefit of exposing an algorithm that will make it easier
  for me to solve the equations in the future.

  For now, this just launches a command window that parses commands.
*)
open System
open Command

(*
Recursively reads input from the console.
Allows the user to input more than one line,
but requires that the user terminates their command with ';'.
The returned string is a multiline string, lines are separated with Environment.NewLine.
*)
let readInput (): string = 
  Console.Write("> ")

  let rec readFromConsole acc =
    let input = Console.ReadLine() + Environment.NewLine
    if input.Trim().EndsWith(";") then
      acc + input
    else readFromConsole (acc + input)
  
  readFromConsole ""

(*
The main loop of the program.
Recursively loops until stopped by the user.
*)
let rec mainLoop (state: Map<string, float32[,]>) =
  let rawInput = readInput ()
  let commandOpt = rawInput |> Command.parseCommand

  let executeCommand cmd: (string * (Map<string, float32[,]>)) =
    match cmd with
    | Exit -> printfn "Exiting..."; exit 0 //I thought about adding another parameter to the method to signify if it should end.
                     //But this would add a bit more complexity, so I went for the dumb solution.
    | Define(name, matrix) -> let matStr = (Environment.NewLine + Matrix.stringify matrix)
                              let output = sprintf "Defined system '%s': %s." name matStr
                              (output, Map.add name matrix state)
    | Solve(name) -> Map.tryFind name state
                     |> Option.map (fun mat -> Solver.solve mat, state)
                     |> Option.defaultWith (fun () -> (sprintf "Unknown system '%s'." name, state))
  commandOpt
  |> Option.map (executeCommand)
  |> Option.defaultWith (fun () -> let unknown = Array.item 0 (rawInput.Split(" "))
                                   (sprintf "Unknown command: '%s'." unknown, state)
                        )
  |> (fun (response, newState) -> printfn "%s" response; mainLoop newState)

[<EntryPoint>]
let main argv =
    printfn "Welcome to LinEqSolver!"

    //The main loop should be defined here.
    //The intention is to make a REPL like experience.
    //We want to be able to parse linear systems as a set of equations
    //Or as matrices.
    mainLoop (Map.empty)

    0 // return an integer exit code
