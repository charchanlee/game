1. Instructions for using the program 
Choosing your players: 
Navigate to Line 52 of Referee.re 
Human + AI (Change the order if you want the AI to go first)
module R1 = Referee(Connect4, HumanConnect4Player, AIConnect4Player);
Human + Human 
module R1 = Referee(Connect4, HumanConnect4Player, HumanConnect4Player);
AI + AI 
module R1 = Referee(Connect4, AIConnect4Player, HumanConnect4Player);

Save the file, run "npm run build" and then "node src/Referee.bs.js" 

Edit the dimensions of the board: Line 46 of Referee.re 
Enter in the number representing the column you would like to insert your piece into, 
1 = from the very left 

2. Overview 
Connect4: This has all the functions for the game Connect4
The Connect4 board is a list of list of ints, in which each list within the main list 
represents a column. 
There are functions that can change the "view" of the board so that the game can detect 
diagonal and horizontal four-in-a-rows, which can help it detect wins 
There are functions that can check whether a move is "legal" by seeing if that column 
has an empty spot 
An empty spot is represented by a 0, pieces inputted by Player1 is represented by a 1. 
and those inputted by Player 2 is represented by a 2
HumanPlayer: This has all the requirements for the user input 
AIPlayer: This has the functions that tells the AI what the best move is based on 
predicting 4 moves ahead 
Referee: This combines the modules and runs a game based on who is playing, 
pieces everything together including the type of game and the type of player 
Is responsible for looping through the game until a terminal state (game over) is reached
3. Possible bugs 
If two AIs are playing each other, the game may throw error as soon as one predicts that there
are no more moves available after 4 moves 
4. Collaborators: 
	TA Darren Fang 
	I didn't work with a partner for this project. 
