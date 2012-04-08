module SHiNKiROU.Minesweeper.MinesweeperShell

open System
open SHiNKiROU.Minesweeper
open SHiNKiROU.Minesweeper.Board
open System.Text.RegularExpressions

type ArgumentFormat =
  | StrO of string
  | IntO of int
  | StrA
  | IntA
  | Any

let (|StartsWith|_|) s (ss : string) = if ss.StartsWith(s) then Some ss else None

// parse an input into a list of arguments, number and string
let readCommand (s : string) =
  let parts' = List.ofArray <| s.Split([|" "; "\t"; "\r"; "\n|"|], StringSplitOptions.RemoveEmptyEntries)
  // no command: simply display the board
  if parts'.Length = 0 then
    "s", [], []
  else
    // handle the special case of no command
    let parts =
      if parts'.Length > 0 && Int32.TryParse(parts'.[0], ref 0) then "o" :: parts' else parts'
    match parts with
    | cmd :: args ->
      cmd, args, List.map (fun x -> try
                                      Some(Int32.Parse x)
                                    with
                                    | :? System.FormatException -> None) args
    | [] -> "", [], []

[<EntryPointAttribute>]
let main (argv : string[]) =
  let board = ref (newBoard 16 16 40)
  let lastSln = ref [| |]

  let running = ref true
  let display = ref true
  let firstClick = ref true

  let polyCheat() =
    let t1 = DateTime.Now
    lastSln := CountingSolver.iteration !board
    let t2 = DateTime.Now
    printfn "Polynomial time solver took %f seconds." (t2.Subtract(t1).TotalSeconds)

  let expCheat() =
    let t1 = DateTime.Now
    lastSln := BacktrackingSolver.iteration !board
    let t2 = DateTime.Now
    printfn "Exponential time solver took %f seconds." (t2.Subtract(t1).TotalSeconds)

  // an action to be performed on the board
  let action f =
    (fun (_ : string list, nargs : int list) ->
       if (!board).State = BoardState.Normal then
         try
           f nargs.[0] nargs.[1] !board |> ignore
           firstClick := false
           display := true
         with
           | Board.BoardException(s) -> printfn "%s" s
       else
         printfn "Please start a new game before performing this operation.")

  // digging a square, ensures the first square is zero, like on most implementations
  let probe1 x y b =
    if !firstClick then
      checkIndex x y b
      board := newBoardNow b.Width b.Height b.Count x y
      firstClick := false
      probe x y !board
    else
      probe x y b

  let helpMessage = "----- SHiNKiROU.Minesweeper.MinesweeperShell -----
A minesweeper solver written in F#.

  - h: show help
  - x: exit game

[New Game]
  - n WIDTH HEIGHT MINE: start a new game with specified options
  - n [beginner|intermediate|expert]: start a new game with this level
  - n: start a new game at Beginner

[Operations]
  To open a square, simply type the coordinate for the squares, note
  the coordinates start at zero, so the top-left square is 0 0, not 1 1.
  - f X Y (or m X Y): Marks or unmarks a flag
  - X Y: Opens a square

[Board Display]
  '.' (dot) is a blank square, ' ' (space) is an unopened square, and
  '#' is a flag.

[Cheats]
  Allows the computer to place some flags and open some squares for you.
  The polynomial time cheat is O(n^3), it works by repeatedly reducing
  statements about the board. The exponential time cheat is O(2^n), it
  works by searching all solutions by backtracking.
  At most times, the polynomial time cheat is sufficient. However,
  positions that only exponential time cheat can solve have been found
  at Expert levels.

  - p: use polynomial time cheat
  - e: use exponential time cheat
  - c: iterate cheats
  - i: show probabilities (after cheat)
  - i X Y: show probability for one square"

  // a list of commands, with argument patterns to match against
  let commands : Map<string, (ArgumentFormat list * (string list * int list -> unit)) list> =
    Map.ofList [
      "h", [[], (fun _ -> printfn "%s" helpMessage) ];
      "x", [[], (fun _ -> running := false) ];
      "s", [[], (fun _ -> display := true)];
      "n", [[IntA; IntA; IntA],
            (fun (args, nargs) ->
              let w, h, n = nargs.[0], nargs.[1], nargs.[2]
              if w < 5 || h < 5 || w > 80 || h > 80 || n < w * h * 10 / 9 + 9 then
                printfn "Invalid board configuration."
              else
                board := newBoard w h n
                firstClick := true
              display := true);
            [StrO("beginner")],
             (fun (args : string list, nargs) ->
                match args.[0] with
                | "intermediate" -> board := newBoard 16 16 40
                | "expert" | "advanced" -> board := newBoard 30 15 100
                | "beginner" | _ -> board := newBoard 8 8 10
                display := true
             );
           ];

     "i", [[IntO(-1); IntO(-1)], (fun (_, nargs) ->
        let probs = !lastSln
        if probs.Length = 0 then printfn "Please use a cheat ('p' or 'e') before using this command."
        let showAll = nargs.[1] = -1
        for p, (x, y) in probs do
          if showAll || (not showAll && (x, y) = (nargs.[0], nargs.[1])) then
            match p with
            | Prob(a, b) -> printfn "(%d, %d) has %d%% (%d/%d) chance of being a mine" x y (a * 100 / b) a b
            | HasMine  -> printfn "(%d, %d) is a mine" x y
            | NoMine -> printfn "(%d, %d) is not a mine" x y) ];

     "c", [[], (fun _ ->
       display := true
       polyCheat()
       while (!board).State = Normal && (!lastSln
                                         |> Array.filter (function
                                                          | Prob(_, _), _ -> false
                                                          | _, _ -> true)).Length > 0 do
         polyCheat()
         if (!board).State = Normal then expCheat()
       )];
     "p", [[], (fun _ -> display := true; polyCheat())];
     "e", [[], (fun _ -> display := true; expCheat())];

     "f", [[IntA; IntA], action toggle];
     "m", [[IntA; IntA], action toggle];

     "o", [[IntA; IntA], action probe1];
     "d", [[IntA; IntA], action probe1];
    ]

  printfn "%s" helpMessage
  while !running do
    if !display then
      printfn "%s" <| textFormat true !board
      printfn "%d/%d Remaining" (minesRemaining !board) (!board).Count

    match (!board).State with
    | BoardState.GameOver -> printfn "----- GAME OVER -----\nType 'n' to start a new game."
    | BoardState.Solved -> printfn "------- FINISH ------\nType 'n' to start a new game."
    | _ -> ()

    display := false

    printf "> "
    let cmd, args, nargs = readCommand (Console.ReadLine())
    if commands.ContainsKey(cmd) then
      let opts = commands.[cmd]

      // find a match (yield true is the hack for breaking from a loop)
      seq {
        for pat, f in opts do
          let args', nargs' =
            List.unzip [
              for i = 0 to pat.Length - 1 do
                let hasMore = i < args.Length
                yield match pat.[i] with
                      | StrO(s) -> if hasMore then args.[i], nargs.[i] else s, None
                      | IntO(s) -> if hasMore && nargs.[i].IsSome then args.[i], nargs.[i] else s.ToString(), Some(s)
                      | StrA -> if hasMore then args.[i], None else "", None
                      | IntA -> if hasMore && nargs.[i].IsSome then args.[i], nargs.[i] else "", None
                      | Any -> if hasMore then args.[i], nargs.[i] else "", None
            ]
          if List.exists ((=) "") args' then
            yield false
          else
            f(args', nargs' |> List.map (fun x -> if x.IsNone then -1 else x.Value))
            yield true
      }
      |> Seq.tryFindIndex id
      |> (fun x -> if x.IsNone then printfn "Invalid argument format for '%s'. Type 'h %s' for help." cmd cmd)
    else
      if cmd <> "" then
        printfn "Command does not exist: '%s' Type 'h' for help." cmd
  0
