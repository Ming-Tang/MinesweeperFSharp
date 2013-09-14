module SHiNKiROU.Minesweeper.GUI

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

// WPF
let appendTo (panel : Panel) uie =
  panel.Children.Add uie |> ignore
  uie

let appendGrid (grid : Grid) x y (uie : #UIElement) =
  let o = appendTo grid uie
  Grid.SetColumn(uie, x)
  Grid.SetRow(uie, y)
  o

let appendGridSpan(grid : Grid) x y sx sy (uie : #UIElement) =
  let o = appendGrid grid x y uie
  Grid.SetColumnSpan(uie, sx)
  Grid.SetRowSpan(uie, sy)
  o

// WPF elements
let window = Window(Title="Minesweeper")

// the direct child of the window
let mainGrid = new Grid(HorizontalAlignment=HorizontalAlignment.Stretch,
                        VerticalAlignment=VerticalAlignment.Stretch)
window.Content <- mainGrid

let auto = GridLength.Auto
let fix x = new GridLength(x)
let star x = new GridLength(x, GridUnitType.Star)

mainGrid.RowDefinitions.Add(new RowDefinition(Height=fix(32.)))
mainGrid.RowDefinitions.Add(new RowDefinition(Height=star(250.)))

mainGrid.ColumnDefinitions.Add(new ColumnDefinition(Width=star(100.))) // board
mainGrid.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength.Auto)) // split
mainGrid.ColumnDefinitions.Add(new ColumnDefinition(Width=star(50.))) // info

(*
    0                    1  2
    +-------------------+-+--------+
 0  | timer button count| |        |
    +-------------------+ |        |
 1  |                   | |  info  |
    |      board        | |  panel |
    |                   | |        |
    +-------------------+-+--------+
*)

let toolbar = new Button(Content="Toolbar") |> appendGrid mainGrid 0 0
let board = new Grid() |> appendGrid mainGrid 0 1

let splitter = new GridSplitter(HorizontalAlignment=HorizontalAlignment.Center,
                                VerticalAlignment=VerticalAlignment.Stretch,
                                ResizeBehavior=GridResizeBehavior.PreviousAndNext,
                                ResizeDirection=GridResizeDirection.Columns,
                                Background=Brushes.Black,
                                Width=5.)
               |> appendGridSpan mainGrid 1 0 1 2
let infoPanel = new Button(Content="Info") |> appendGridSpan mainGrid 2 0 1 2

let onInit() =
  board.Children.Clear()
  board.RowDefinitions.Clear()
  board.ColumnDefinitions.Clear()

  for i in 0..9 do
    board.RowDefinitions.Add(new RowDefinition(Height=star(1.)))
  for j in 0..9 do
    board.ColumnDefinitions.Add(new ColumnDefinition(Width=star(1.)))
  for i in 0..9 do
    for j in 0..9 do
      let btn = new Button(Content="+") |> appendGrid board i j
      ()

[<EntryPoint>]
[<STAThread>]
let main (argv : string[]) =
  onInit()
  (Application()).Run window |> ignore
  0
