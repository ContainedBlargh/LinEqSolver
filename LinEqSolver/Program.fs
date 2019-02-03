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


type Step = Solver.Step

(*
The main loop of the program.
Recursively loops until stopped by the user.
*)
let rec mainLoop (matrices: Map<string, float32[,]>) (steps: Map<string, Step list>): unit =
  let rawInput = readInput ()
  let commandOpt = rawInput |> Command.parseCommand

  let addStep (name: string) (step: Step): Map<string, Step list> = 
    Map.tryFind name steps 
    |> Option.defaultWith (fun () -> [])
    |> fun oldSteps -> Map.add name (oldSteps@[step]) steps
  
  let execUpdate name solvFun resStr = Option.map (fun mat -> let (nMat, step) = solvFun mat in
                                                              let nMatrices = Map.add name nMat matrices in
                                                              let nSteps = addStep name step in
                                                                (resStr + Environment.NewLine + Matrix.stringify nMat, nMatrices, nSteps))

  let execDefault name = Option.defaultWith (fun () -> (sprintf "Unknown system '%s'." name, matrices, steps))

  let executeCommand (cmd: Command) =
    match cmd with
    | Exit -> printfn "Exiting..."; exit 0 //I thought about adding another parameter to the method to signify if it should end.
                      //But this would add a bit more complexity, so I went for the dumb solution.
    | Define(name, matrix) -> let matStr = (Environment.NewLine + Matrix.stringify matrix)
                              let output = sprintf "Defined system '%s': %s." name matStr
                              (output, Map.add name matrix matrices, steps)
    | Trace(name) -> Map.tryFind name steps |> Option.defaultWith (fun () -> []) |> Solver.stepsToString |> fun s -> (s, matrices, steps)
    | Check(name) -> Map.tryFind name matrices
                     |> Option.map (fun mat -> let check = (Solver.check mat) in (sprintf "The system '%s' is %s" name check, matrices, steps))
                     |> execDefault name
    | AddRowTo(name, r1, s, r2) -> Map.tryFind name matrices
                                   |> execUpdate name (fun mat -> Solver.addRowTo r1 s r2 mat) (sprintf "Added %d * %0.2f to %d in '%s'" r1 s r1 name)
                                   |> execDefault name
    | ScaleRow(name, r, s) -> Map.tryFind name matrices
                              |> execUpdate name (fun mat -> Solver.scaleRow r s mat) (sprintf "Scaled %d with %0.2f in '%s'" r s name)
                              |> execDefault name
    | SwapRows(name, r1, r2) -> Map.tryFind name matrices
                                |> execUpdate name (fun mat -> Solver.swapRows r1 r2 mat) (sprintf "Swapped %d and %d in %s" r1 r2 name)
                                |> execDefault name
  commandOpt
  |> Option.map (executeCommand)
  |> Option.defaultWith (fun () -> let unknown = Array.item 0 (rawInput.Split(" "))
                                   (sprintf "Unknown command: '%s'." unknown, matrices, steps)                   )
  |> (fun (response, newMatrices, newSteps) -> printfn "%s" response; mainLoop newMatrices newSteps)


[<EntryPoint>]
let main argv =
    printfn "Welcome to LinEqSolver!"

    //The main loop should be defined here.
    //The intention is to make a REPL like experience.
    //We want to be able to parse linear systems as a set of equations
    //Or as matrices.
    mainLoop Map.empty Map.empty

    0 // return an integer exit code
