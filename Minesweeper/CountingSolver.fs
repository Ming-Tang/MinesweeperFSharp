module SHiNKiROU.Minesweeper.CountingSolver

open SHiNKiROU.Minesweeper.Board

(* Counting Solver *)

/// Provides statements about the board in the format of (NUMBER OF MINES, AMONG THESE SQUARES)
let index (board : Board) =
  let flags = board.Flags
  let bboard = board.Board

  // gather information about the board
  let mutable cs : Set<Number * Set<int * int>> =
    Set.ofSeq
    <| seq {
      for x in 0..board.Width - 1 do
        for y in 0..board.Height - 1 do
          let n = bboard.[x, y]
          // check a numbered square
          if flags.[x, y] = Open && n > 0uy && n < 9uy then
            // c = number of mines, list of squares around the number
            let c =
              // for each squares around the number
              around board x y
              |> Seq.map (fun (x, y) ->
                match flags.[x, y] with
                | Flag -> true, None // deduct for flags
                | Close -> false, Some(x, y) // a closed square is a candidate square
                | Open -> false, None
              )
              |> Seq.fold (fun (x, ts) (b, o) ->
                // deduct for flags, and add to candidate list
                (if b then x - 1uy else x), (
                  match o with
                  | Some(t) -> t :: ts
                  | None -> ts
                )
              ) (n, [])
              |> (fun (a, b) -> a, Set.ofList b)
            match c with // add to the set only of the candidate list is non-empty
            | _, b -> if b <> Set.empty then yield c
    }

  // keep applying the subset rule until no more changes happen
  let mutable changed = true
  // O(n^3) loop
  while changed do
    changed <- false
    for e1 in cs do
      for e2 in cs do
        if e1 <> e2 then
          let (n1, s1), (n2, s2) = e1, e2
          // if S1 subset S2: S3 = S2 - S1 and N3 = N2 - N1
          if Set.isProperSubset s1 s2 then
            let c = (n2 - n1, Set.difference s2 s1)
            if not <| Set.contains c cs then
              cs <- Set.add c cs
              changed <- true
  cs

/// Find a set of squares by counting
let counting (probs : bool) (board : Board) (idx : Set<Number * Set<int * int>>) =
  let yes x = seq { while true do yield x }
  seq {
    for n, ss in idx do // n: number of mines, ss: list of squares in the set
      if ss.Count <> 0 then
        if n = 0uy then
          yield! Seq.zip (yes NoMine) ss // all open
        elif n = byte ss.Count then // all mines
          yield! Seq.zip (yes HasMine) ss
        elif probs && n < byte ss.Count then // get probability
          yield! Seq.zip (yes <| Prob(ss.Count - int n, ss.Count)) ss
        elif n > byte ss.Count then // this should not happen
          assert false
  }
  |> Set.ofSeq
  |> Array.ofSeq

/// solution set
let solutions (board : Board) = index board |> counting true board

/// determines the solution and modifies the board
let iteration (board : Board) =
  let sln = solutions board
  useSolution board sln
  sln
