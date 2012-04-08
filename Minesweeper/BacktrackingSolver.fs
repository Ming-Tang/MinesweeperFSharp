module SHiNKiROU.Minesweeper.BacktrackingSolver

open SHiNKiROU.Minesweeper.Board

#region "Types"

exception TestFail

/// A dependency statement: squares in the Set<Coordinate> depends on the Coordinate
type Entry = Coordinate * Set<Coordinate>
type Index = Entry[]
type Flag = bool

type VirtualMark =
  | Unknown
  | NoFlag
  | Flag

/// A board with only information given to the player
type VirtualBoard = VirtualMark[,]

/// A list of booleans (true for mine and false for no mine),
/// representing a solution, indexed by an Index
type Solution = bool[]

#endregion

(*

      0   1   2
  0 [#0][#1][#2]
  1 [ 1][ 2][ 2]

              | #0    | #1    | #2    |             | array index
              |-------+-------+-------|             |
 Index      : | 0, 0  | 1, 0  | 2, 0  | Coordinate  | an Unknown square
              |-------+-------+-------|             |
 Entry      : | 0, 1  | 0, 1  |       |\            | set of dependent
              | 1, 1  | 1, 1  | 1, 1  | > Set       | squares around it
              |       | 2, 1  | 2, 1  |/            |
              |-------+-------+-------|             |
 Solution   : | false | true  | true  | bool        | the solution slots

  For example, the solution slot #1 (sln.[1]) maps
  to the coordinate (1, 0) (index.[1] = (1, 0))

 * there may be multiple solutions

*)

// TODO break down the board into independent components to reduce time

/// Provides statements about the board in the format of (COORD OF UNKNOWN, DEPENDENT SQS),
/// which serves as an index for solution slots.
let index (board : Board) : Index =
  let bboard = board.Board
  let flags = board.Flags
  seq {
    for x = 0 to board.Width - 1 do
      for y = 0 to board.Height - 1 do
        if flags.[x, y] = Board.Close then
          let set =
            Board.around board x y
            |> Seq.filter (fun (x, y) -> // find dependent squares
              // filter for open and numbered squares
              flags.[x, y] = Board.Open && bboard.[x, y] > 0uy && bboard.[x, y] < 9uy
            )
            |> Set.ofSeq
          if set <> Set.empty then
            // yield for only non-empty sets (if the set is empty, it have no
            // useful information to lead to the solution)
            yield (x, y), set
  }
  |> Array.ofSeq

/// Find a solution by backtracking.
let backtrack (findOne : bool) (board : Board) (idx : Index) =
  if idx.Length = 0 then
    idx, []
  else
    let ls = idx.Length
    let idx = idx
    let sln = Array.create ls false
    let mutable slns = []
    let mutable ptr = 0
    let mutable cont = true

    let width = board.Width
    let height = board.Height
    let flags = board.Flags
    let bboard = board.Board
    let remaining = minesRemaining board

    // get cooresponding coordinate from a slot index
    let aCoord ptr = fst idx.[ptr]

    // a test board
    let vBoard = Array2D.init width height (fun x y ->
      match flags.[x, y] with
      | Board.Flag -> Flag
      | Board.Open -> NoFlag
      | Board.Close -> Unknown
    )

    (*
      Non-recursive backtracking algorithm

        sln [ false, false, true, false, false, true, false, true ]
                            ^
                        idx = 3

      while there are more branches to visit (ptr > -1):
        backtrack <- false (if backtrack is true, then backtracking happens)
        if partial solution matches:
          if solution is complete (ptr reached the end of sln array):
            add solution to solution list
            if there is another branch to visit:
              advance (go to the next branch, in this type of problem: sln.[ptr] = true)
            else
              backtrack <- true
        else
          if there is another branch to visit (sln.[ptr] = false):
            advance (sln.[ptr] = true)
          else
            backtrack <- true

        if backtrack:
          while sln[ptr] is a dead end (sln.[ptr] = true)
            ptr --
          advance
    *)
    while ptr > -1 do
      let mutable doBack = false

      let cx, cy = aCoord ptr
      if (
          // test partial solution
          let i = ptr
          let coord, numsqs = idx.[i]
          let numsqs' = Array.ofSeq numsqs
          let mutable loop = true // true to keep looping
          let mutable result = true // result of the test
          let mutable j = 0 // loop counter
          // this is a crude for loop written as while to make breaking possible
          // for each of the associated squares
          while loop do
            let x, y = numsqs'.[j]
            // check if the partial solution matches
            let mutable num = int bboard.[x, y]
            let mutable unknowns = 0
            let mutable noflags = 0
            let mutable flags = 0
            for x1, y1 in Board.around board x y do
              match vBoard.[x1, y1] with
              | Unknown -> unknowns <- unknowns + 1
              | NoFlag -> noflags <- noflags + 1
              | Flag -> flags <- flags + 1
            //let expected = flags + noflags + unknowns - num

            j <- j + 1

            (* fail conditions
               1: too many flags (determined with the given number of mines)
               2: too many flags
               3: not enough indet squares to complete the solution
               4: not enough slots to fill all unknowns
            *)
            if remaining - (Array.filter id sln.[0..ptr]).Length < 0
               || flags > num || num - flags > unknowns
               || unknowns > ls - ptr - 1 then
              result <- false
              loop <- false
            if j >= numsqs'.Length then loop <- false
          result)
      then
        // searched in the end: solution accepted
        if ptr = ls - 1 then
          slns <- Array.copy sln :: slns
          if sln.[ptr] then
            // backtrack if no other alternative is considered
            //  [false, true, true] -> [false, true, true]
            //                ^         ^
            // OR
            // [false, true, false] -> [false, true, true]
            doBack <- true
          else
            // seek next alternative
            sln.[ptr] <- true
            vBoard.[cx, cy] <- Flag
        else
          // advance partial solution pointer
          ptr <- ptr + 1
          let cx, cy = aCoord ptr
          sln.[ptr] <- false
          vBoard.[cx, cy] <- NoFlag
      else
        if sln.[ptr] then
          // backtrack if all branches are searched
          doBack <- true
        else
          // go to next branch
          sln.[ptr] <- true
          vBoard.[cx, cy] <- Flag

      // backtrack
      if doBack then
        // go back to last branch
        while ptr > -1 && sln.[ptr] do
          // [true, false, true, true, true] -> [true, false, true, true, true]
          //                           ^               ^
          let cx, cy = aCoord ptr
          // and restore working board state
          vBoard.[cx, cy] <- Unknown
          ptr <- ptr - 1
        if ptr > -1 then
          // and advance
          let cx, cy = aCoord ptr
          sln.[ptr] <- true
          vBoard.[cx, cy] <- Flag

      // bail out if findOne is true and found two or more solutions
      if findOne && slns.Length > 0 then
        ptr <- -1

    idx, slns

let backtrackAll = backtrack false
let backtrackOne = backtrack true

/// Find squares that is 100% sure about what is under there.
let commons (slns : Solution list) : bool[] =
  let slns = slns
  let sln = slns.[0]
  [|
    for i in 0..sln.Length - 1 ->
      seq {
        for j in 0..slns.Length - 1 -> slns.[j].[i]
      }
      |> Seq.forall ((=) sln.[i])
  |]

/// Count the probability of mines, based on the formula
/// prob of mines of a square = number of mines / number of solutions
let slnlist (probs : bool) (slns : Solution list) (index : Index) : SolutionList =
  let slns = slns
  if slns.Length = 0 then [| |]
  else
    let sln = slns.[0]
    let n = slns.Length
    seq {
      for i in 0..sln.Length - 1 do
        let m =
          // count mines per index
          seq {
            for j in 0..slns.Length - 1 -> slns.[j].[i]
          }
          |> Seq.filter id
          |> Seq.length
        let co = fst index.[i]
        if m = 0 then yield NoMine, co
        elif m = n then yield HasMine, co
        elif probs then yield Prob(m, n), co
    }
    |> Array.ofSeq

let solutions (board : Board) : Index * (Solution list) = index board |> backtrackAll board

let iteration (board : Board) : SolutionList =
  let index, slns = solutions board
  let sln = slnlist true slns index
  useSolution board sln
  sln
