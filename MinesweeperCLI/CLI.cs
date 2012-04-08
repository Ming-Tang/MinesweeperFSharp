using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SHiNKiROU.Minesweeper;

namespace SHiNKiROU.MinesweeperCLI {
  class CLI {
    public static void Main(String[] args) { main(args); }

    //

    public static void main(String[] args) {
      args = new String[] {"12", "12", "30", "p", "2", "2"};
      int width = 16, height = 16, count = 40;
      int i = 0;
      var cmd = "";
      var cargs = new List<String>();
      foreach (String arg in args) {
        int n;
        if (i == 4) {
          cargs.Add(arg);
        } else if (i == 3) {
          cmd = arg + "";
          i ++;
        } else if (Int32.TryParse(arg, out n)) {
          switch (i) {
            case 0:
              width = n;
              break;
            case 1:
              height = n;
              break;
            case 2:
              count = n;
              break;
          }
          i ++;
        }
      }
      var b = Board.newBoard(width, height, count);
      while (true) {
        Console.WriteLine(Board.textFormat(true, b));
        Console.Write("{0}/{1}> ", Board.toBeFlagged(b), b.Count);

        var arr = Console.ReadLine().Split(
          new char[] { ' ', '\t' },
          StringSplitOptions.RemoveEmptyEntries
        );
        cargs = new List<String>(arr);
        cmd = "";
        if (arr.Length > 0) {
          cmd = arr[0];
          cargs.RemoveAt(0);
        }

        var nargs = new int[cargs.Count];
        for (i = 0; i < cargs.Count; i ++) {
          int n = 0;
          if (Int32.TryParse(cargs[i], out n)) {
            nargs[i] = n;
          } else {
            nargs[i] = Int32.MinValue;
          }
        }

        switch (cmd) {
          case "e":
          case "x":
          case "exit":
          case "q":
          case "quit":
            return;
          case "p":
          case "probe":
          case "o":
          case "open":
          case "d":
          case "dig":
            var a = Board.probe(nargs[0], nargs[1], b);
            b = a.Item1;
            break;
          case "f":
          case "flag":
            b = Board.flag(nargs[0], nargs[1], b);
            break;
        }

        if (b.State == Board.BoardState.Solved) {
          Console.WriteLine("Congrats! You solved the game!");
        } else if (b.State == Board.BoardState.GameOver) {
          Console.WriteLine("Game Over!");
        }

        if (b.State != Board.BoardState.Normal) {
          b = Board.newBoard(width, height, count);
        }
      }
    }
  }
}
