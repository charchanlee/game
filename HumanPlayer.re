open Game;
open Connect4; 
open Player; 

/* Data Definition:
whichPlayer: tells you which player's turn it is, P1 or P2 
status: tells you whether someonee has won the game. Win(player) indicates
that player has won the game, Draw indicates a draw, and Ongoing(player) indicates
that it's player's turn and the game is not over. 
state: includes the status and the representation of the connect4 board in terms 
of a list of list of ints. 
move: indicates which column the piece is intended to be inserted in */

module HumanPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

  type getInput = {
    mutable m: PlayerGame.move
  }

  let getInputJSLine : unit => string = {
    [%bs.raw{|
      function() {
        const readlineSync = require('readline-sync');
        const rl = readlineSync;
        var ans = rl.question('What move do you want to make? ');
        return ans;
    }|}]
  }


  let rec nextMove = s => {
    let rec isLegalP: (list(PlayerGame.move), PlayerGame.move) => bool = 
    (moves, choice) => 
      switch (moves) {
        | [] => false 
        | [hd,...tl] => (hd == choice) ? true : isLegalP(tl, choice)
      };

    let myMove : getInput = {m: List.nth(PlayerGame.legalMoves(s), 0)};
    let input : string = getInputJSLine();
    switch (input) {
    | "exit" => failwith("Exiting Game REPL"); 
    | _ => {
      let mov = try(PlayerGame.moveOfString(input)) {
        | _ =>
          print_endline("not a valid move");
          nextMove(s);
        }
        /* check this against the list of legal moves */
        if (isLegalP(PlayerGame.legalMoves(s), mov)) {
          myMove.m = mov;
        } else {
          print_endline("Error: Human Player made illegal move");
          myMove.m = nextMove(s);
        }
      }
    }
    myMove.m;
  };

  let playerName = "HumanPlayer"
};

module TestGame = Connect4; 
module HumanConnect4Player = HumanPlayer(Connect4); 

module TestHumanPlayer = HumanPlayer(TestGame); 
module MyHumanPlayer: Player = TestHumanPlayer;
open TestHumanPlayer; 

/* Because next_move involves read input, you don't need to test it here.
 * If you use any helper procedures in TestHumanPlayer that don't take in 
 * or return a state, test them here. */
