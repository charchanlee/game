open CS17SetupGame;
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

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

  let nextMove: PlayerGame.state => PlayerGame.move = s => 
    { 
      
        /* argMax: 
            input: a nonempty list, lst, of items of type 'a; 
                    a scoring function f, from 'a -> float 
            output: an item s in the list for which f(s) is the largest
            */
        let rec argMax: (list('a), 'a => float) => ('a, float) = (lst, f) => 
          switch (lst) {
          | [] => failwith("Domain error")
          | [a] => (a, f(a)) 
          | [hd,...tl] => 
            {
              let (rn, rf) = argMax(tl, f); 
              let hdf = f(hd); 
              (rf > hdf) ? (rn, rf) : (hd, hdf) 
            }
          };

        /* argMin: 
          input: a nonempty list, lst, of items of type 'a; 
                  a scoring function f, from 'a -> float 
          output: an item s in the list for which f(s) is the least
          */
        
        let argMin: (list('a), 'a => float) => ('a, float) = (lst, f) => 
          argMax(lst, x => -. f(x)); 
        /* value: 
          input: a state, s, a move, m, and k, the number of depths to look ahead
          output: the value of optimally making k-1 more moves after m from state 
          */
        let rec value: (PlayerGame.state, PlayerGame.move, int) => 
        float = (s, m, k) => 
          {
            let resultState = PlayerGame.nextState(s, m); 
            switch (PlayerGame.gameStatus(resultState)) { 
              | Win(P1) => infinity
              | Win(P2) => neg_infinity
              | Draw => 0.
              | Ongoing(P1) => 
                if (k == 0)
                {PlayerGame.estimateValue(resultState)}
                else {snd(argMax(PlayerGame.legalMoves(resultState), 
                  x => value(resultState, x, k - 1)))};
              | Ongoing(P2) => 
                if (k == 0)
                {PlayerGame.estimateValue(resultState)}
                else {snd(argMin(PlayerGame.legalMoves(resultState), 
                  x => value(resultState, x, k - 1)))};   
            };
          };
        /* minimax
            input: a game tree represented by top node s, with values at each 
            terminal  state 
            output: a (float, option(move)) pair, where the float is the value of 
              the game to P1 if everyone moves optimally at each state, and the 
              move option is the optimal move (if any) for whichever player is 
              supposed to move at the  state s. */ 
        let minimax: PlayerGame.state => (option(PlayerGame.move), float) = s => 
          {   
              let k = 4; 
              let lstMoves = PlayerGame.legalMoves(s); 
              if (lstMoves == []) 
              {(None, PlayerGame.estimateValue(s))}
              else {
                switch (PlayerGame.gameStatus(s)) {
                | Ongoing(P1) => 
                  {
                    let (bestMove, scoreMove) = 
                      argMax(lstMoves, m => value(s, m, k)); 
                    (Some(bestMove), scoreMove); 
                  }
                | Ongoing(P2) => 
                  {
                    let (bestMove, scoreMove) = 
                      argMin(lstMoves, m => value(s, m, k)); 
                    (Some(bestMove), scoreMove); 
                  }
                };
              }; 
             
          };
        

        switch (fst(minimax(s))) {
          | None => failwith("No next moves")
          | Some(bestMove) => bestMove; 
          };
      
    }
  /* put your team name here! */
  let playerName = "Charlotte Lee"; 
}; 


module TestGame = Connect4;
module AIConnect4Player = AIPlayer(Connect4); 

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer: Player = TestAIPlayer;
open TestAIPlayer; 


