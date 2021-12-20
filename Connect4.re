open CS17SetupGame;   
open Game; 

/* Data Definition:
whichPlayer: tells you which player's turn it is, P1 or P2 
status: tells you whether someonee has won the game. Win(player) indicates
that player has won the game, Draw indicates a draw, and Ongoing(player) indicates
that it's player's turn and the game is not over. 
state: includes the status and the representation of the connect4 board in terms 
of a list of list of ints. 
move: indicates which column the piece is intended to be inserted in */

module Connect4 = {
    type whichPlayer = 
    | P1 
    | P2; 

    type status = 
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer); 

    type state = (status, list(list(int))); 
    type move = int;  
    /* transpose: 
        input: a list of list of ints, original, representing the original board
          in which each list inside the main list is a column, left to right 
          representing bottom to top
        output: a list of list of ints representing the same board, 
        reorganized so that the list of list of ints 
        is rows, the first list being the bottom row and the last being the top
        
        recursive diagram: 
        oi: [[1, 2, 3] [4, 5, 6] [7, 8, 9]]
        ri: [[4, 5, 6] [7, 8, 9]]
        ro: [[4, 7,] [5, 8] [6, 9]]
        oo: [[1, 4, 7,] [2, 5, 8] [3, 6, 9]]

        oi: [[0, 2, 3] [0, 5, 6] [0, 8, 9]]
        ri: [[4, 5, 6] [7, 8, 9]]
        ro: [[4, 7,] [5, 8] [6, 9]]
        oo: [[0, 0, 0,] [2, 5, 8] [3, 6, 9]]
         */
    let rec transpose: list(list(int)) => list(list(int)) = original => 
        switch (original) {
            | []
            | [[], ..._] => failwith("Matrix cannot be 0-dimensional")
            | [[_], ..._] => [List.flatten(original)]
            | [[_, ..._], ..._] =>
                [List.map(List.hd, original),...
                  transpose(List.map(List.tl,original))]
        };
    /* consecutive: 
        input: a list of ints, lst, and an int, i, 
        output: a bool, and an option(whichPlayer), true if there are i number
        of consecutive pieces from one player, with Some(player)
          and (false, None) if there are not i consecutive pieces found in lst*/
    let rec consecutive: (list(int), int) => (bool, option(whichPlayer)) = 
    (lst, i) => 
      {
        let oGoal = i; 
        let rec consecHelper: (list(int), int, int) => (bool, option(whichPlayer))
        = (lst, goal, counter) => 
        switch (lst) {
          | [] => (false, None)
          | [fst] => (false, None)
          | [fst, snd,..._] when ((fst == snd) && (fst == 1 || fst == 2)) => 
            if (goal == counter + 2)
            {(true, (fst == 1) ? Some(P1) : Some(P2))}
            else {consecHelper(List.tl(lst), goal, counter + 1)}; 
          | [fst, snd, ..._] when ((fst != snd) || (fst == 0)) => 
            consecHelper(List.tl(lst), oGoal, 0)
        };

        consecHelper(lst, i, 0); 
      }
 
    /* diags: 
      input: a list of list of ints representing the original board, in which 
      each list within the main list is a column of pieces, the first being
      the bottom and the last being the top 
      output: a list of list of ints representing the diagonal view: each list
      within the main list is the top left to bottom left view of the board , 
      ordered from left to right 
      
      recursion diagrams: 
      oi: [[1, 3, 0, 2], [5, 9, 8, 6], [4, 1, 7, 7]]
      ri: [[5, 9, 8, 6], [4, 1, 7, 7]]
      ro: [[5], [9, 4], [8, 1], [6, 7], [7]]
      oo: [[1], [3, 5], [0, 9, 4], [2, 8, 1], [6, 7], [7]]

      oi: [[0, 3, 0, 2], [5, 9, 8, 6], [4, 1, 7, 0]]
      ri: [[5, 9, 8, 6], [4, 1, 7, 0]]
      ro: [[5], [9, 4], [8, 1], [6, 7], [7]]
      oo: [[0], [3, 5], [0, 9, 4], [2, 8, 1], [6, 7], [0]]*/

    let rec diags: list(list(int)) => list(list(int)) = original => 
        {
            let rec pairUp: (list(int), list(list(int))) => list(list(int)) = 
            (lst, original) => 
            switch (lst, original) {
                | ([], _) => original 
                | ([hd, ...tl], [diag1, ...otherDiags]) => 
                  [[hd, ...diag1],...pairUp(tl, otherDiags)]
                | _ => failwith("Domain error")
            };
      
            switch (original) {
                | [] => failwith("Domain error")
                | [a] => List.map(x => [x], a)
                | [lst,...tl] => pairUp(lst, [[], ...diags(tl)])
            }; 
        } 
    /* stringofPlayer: 
      input: whichPlayer, indicating the player 
      output: a string representation of the player */
    let stringOfPlayer: whichPlayer => string = player => 
      switch (player) {
        | P1 => "Player 1"
        | P2 => "Player 2"
      };
    /* stringOfState: 
      input: a state, s
      output: a string representation of the board during that state, so that 
      a user can understand and imagine a connect4 board visually*/
    let stringOfState: state => string = s => 
      { 
        let rec printRow: list(int) => string = input => 
          switch (input) {
          | [] => ""
          | [x] => string_of_int(x) 
          | [hd, ...tl] => string_of_int(hd) ++ printRow(tl)
          };
        let rec printBoard: list(list(int)) => string = input => 
            switch (input) {
            | [] => ""
            | [x] => printRow(x)
            | [hd,...tl] => printRow(hd) ++ "\n" ++ printBoard(tl)
            };

            printBoard(List.rev(transpose(snd(s))));
      };
    
    /* stringOfMove: 
      input: a move, moveNum 
      output: a string representation of the move */
    let stringOfMove: move => string = moveNum => 
      string_of_int(moveNum);  
    /* initialState: 
      input: a string representing the dimensions of the initial board set up, 
      two ints separated by a space 
      output: a state representation of the initial board set up, with all zeroes
      as the board is entirely empty */
    let initialState: string => state = s =>
        {
          let boardDims = parseBoardDims(s);
          let boardHeight = getBoardHeight(boardDims);
          let boardWidth = getBoardWidth(boardDims);

          let rec createBoard: (int, int) => list(list(int)) = 
          (width, height) => 
            {
              let rec createColumn: int => list(int) = height => 
                switch (height) {
                    | 0 => []
                    | _ => List.append(createColumn(height - 1),[0])
                };
              
              switch (width) {
                  | 0 => [] 
                  | 1 => [createColumn(height)]
                  | _ => List.append(createBoard(width - 1, height), 
                    [createColumn(height)]) 
              };
            };
          (Ongoing(P1), createBoard(boardWidth, boardHeight)); 
        }; 
    
    /* legalMoves: 
      input: a state, currentState, representing the current state
      output: a list of moves, which has all the legal moves available from that 
      state. only the column numbers which is within the board dimensions 
      and are empty */
    let legalMoves: state => list(move) = currentState => 
        {
            /* legalMovesHelper: 
                input: a list of list of ints, representing the board; 
                        an int, i, the counter for the current column 
                output: a list of all columns that still have an empty spot */
      
            let rec legalMovesHelper: (list(list(int)), int) => list(move) = 
            (board, i) =>
                {
                  /* containsSpotP: 
                      input: a list of ints, column 
                      output: a bool, true, if the column has a zero, false 
                          otherwise  */
                  let rec containsSpotP: list(int) => bool = column => 
                      switch (column) {
                          | [] => false
                          | [0] => true 
                          | [a,..._] => 
                            (a == 0)? true : containsSpotP(List.tl(column))
                      };
                  switch (board) {
                      | [] => []
                      | [column,..._] => (containsSpotP(column)) ?
                        List.append([i], legalMovesHelper(List.tl(board), i + 1)) : 
                        legalMovesHelper(List.tl(board), i + 1)
                  }; 
                };
      
            switch (fst(currentState)) {
                | Win(_)
                | Draw => failwith("Game over, no legal moves remaining")
                | Ongoing(_) => legalMovesHelper(snd(currentState), 1)
            }; 
        }
    /* gameStatus: 
      input: a state, currentState 
      output: the status of the state */
    let gameStatus: state => status = currentState => 
      fst(currentState); 
    /* nextState: 
      input: a state, s, and a move, m 
      output: the state representation of the state that would result from 
      applying m to s */

      /* recursion diagrams:  */
    let nextState: (state, move) => state = (s, m) => 
      {
        /* addPieceWhere: 
          input: a list of list of ints, board, representing the board; 
                a move, representing the legal move to be made 
                a whichPlayer, representing the player making the move 
          output: the changed board with the player's piece added, represented
                by a 1 if it's P1, and a 2 if it's P2 */
        let rec addPieceWhere: (list(list(int)), move, whichPlayer) => 
        list(list(int)) = (board, moveNum, player) => 
            {
              let rec addPieceHere: (list(int), whichPlayer) => list(int) = 
              (column, player) => 
                  switch (column, player) {
                      | ([], _) => [] 
                      | ([hd, ...tl], P1) => 
                        (hd == 0)? [1,...tl] : [hd, ...addPieceHere(tl, player)]
                      | ([hd, ...tl], P2) => 
                        (hd == 0)? [2,...tl] : [hd, ...addPieceHere(tl, player)]
                  };
              switch (board) {
                  | [column] when (moveNum == 1) => [addPieceHere(column, player)] 
                  | [column,...tl] => if ((moveNum) == 1)
                      {[addPieceHere(column, player),...tl]}
                      else {[column,...addPieceWhere(tl, moveNum - 1, player)]}; 
              };
            }; 
        let rec determineWin: (list(list(int))) => bool = board => 
          {
            let rec columnWin: list(list(int)) => bool  = board => 
              switch (board) {
                  | [column] => fst(consecutive(column, 4))
                  | [column, ...tl] => 
                    fst(consecutive(column, 4)) || columnWin(tl)
              }; 
            let rowWin: list(list(int)) => bool = board => 
                columnWin(transpose(board)); 
            let diagWin: list(list(int)) => bool = board => 
                columnWin(diags(board)) || columnWin(diags(List.rev(board)));  

            (columnWin(board) || rowWin(board) || diagWin(board)); 
           
          }; 
        let rec isLegalP: (list(move), move) => bool = (moves, choice) => 
          switch (moves) {
            | [] => false 
            | [hd,...tl] => (hd == choice) ? true : isLegalP(tl, choice)
          };     
        let (Ongoing(player), oldBoard) = s; 
        let l1 = legalMoves(s); 
        let newBoard = addPieceWhere(oldBoard, m, player); 
        let ns = (Ongoing((player == P1)? P2 : P1), newBoard); 
        if (isLegalP(l1, m)) 
        {
          if (determineWin(newBoard)) {
            (Win(player), newBoard)
          } 
          else {
            if (legalMoves(ns) == []) {
                (Draw, newBoard)
            } 
            else {
              (Ongoing((player == P1)? P2 : P1), newBoard)
            }; 
          };
        }
        else {failwith("Error: nextState was attempted with illegal move")};
      }



    
    /* moveOfString: 
      input: a string representation of a move, input
      output: a move representation of the input move */
    let moveOfString: string => move = input => 
      try(int_of_string(input)) {
        | _ => failwith("Try again"); 
        }; 
       
    

    /* estimateValue: 
      input: a state, s
      output: a float, which is the estimated value of the state to P1, more 
      positive if it is more valuable and negative or less positive if it is 
      more valuable to P2 */
    let estimateValue: state => float = s => 
        switch (gameStatus(s)) {
            | Win(P1) => infinity 
            | Win(P2) => neg_infinity 
            | Draw => 0. 
            | Ongoing(player) => 
                {
                  let board = snd(s); 
                  let rec c3Board: list(list(int)) => float = board => 
                    switch (board) {
                        | [] => 0.
                        | [hd, ...tl] => 
                            switch (consecutive(hd, 3)) {
                            | (true, Some(P1)) => c3Board(tl) +. 5.**3.
                            | (true, Some(P2)) => c3Board(tl) -. 5.**3.  
                            | (false, None) => c3Board(tl) 
                            }; 
                    }; 
                      
                  let rec c2Board: list(list(int)) => float = board =>
                    switch (board) {
                        | [] => 0.
                        | [hd, ...tl] => 
                            switch (consecutive(hd, 2)) {
                                | (true, Some(P1)) => c2Board(tl) +. 5.**2.
                                | (true, Some(P2)) => c2Board(tl) -. 5.**2.
                                | (false, None) => c2Board(tl) 
                            }; 
                    };
                  let rec oneB: list(int) => float = column => 
                  switch (column) {
                    | [] => 0.
                    | [1, 1, 0, 1, ..._] 
                    | [1, 0, 1, 1, ..._] => oneB(List.tl(column)) +. 5.**3.
                    | [2, 2, 0, 2, ..._] 
                    | [2, 0, 2, 2, ..._] => oneB(List.tl(column)) -. 5.**3.
                    | [_, ..._] => oneB(List.tl(column))
                  };
                  let rec bigOneB: list(list(int)) => float = board => 
                    switch (board) {
                      | [] => 0.
                      | [hd] => oneB(hd)
                      | [hd, ...tl] => oneB(hd) +. bigOneB(tl)
                    }; 
                   
                    
                  let rowView = transpose(board); 
                  let diagView1 = diags(board); 
                  let diagView2 = diags(List.rev(board)); 
                
                  c3Board(board) +. c3Board(rowView) +. 
                  c3Board(diagView1) +. c3Board(diagView2) +. 
                  c2Board(board) +. c2Board(rowView) +.
                  c2Board(diagView1) +. c2Board(diagView2) +. 
                  bigOneB(board) +. bigOneB(diagView1) +. 
                  bigOneB(diagView2) +. bigOneB(rowView); 
                }
        };
};


module MyGame: Game = Connect4;
open Connect4;

