# Isaac Board Game

## Authors

### Group Isaac_3

- [Francisco Miguel Correia Mariano Pinheiro Cardoso](https://github.com/FranciscoCardoso913), up202108793, 50%
- [José António Pereira Martins](https://github.com/ZeAntonioM), up202108794, 50%

## Run the game

- Open Sicstus Prolog
- run `consult('isaac.pl').`
- run `play.`

## Description

*Isaac* is a strategic board game implemented in Prolog, originally created in 2010. The game features two phases: placement and scoring. The objective is to reach 100 points during the scoring phase or secure the highest score.

### Game Rules

#### Board

Isaac utilizes a 10x10 board with unique numerical values for each player. Players find numbers from 1 to 10 on their board's bottom and 00 to 90 on the left side. The board starts empty. Players have 15 pieces of various sizes, each with its point value.

##### Placement Phase

The game begins with the placement phase. Players take turns placing their pieces on empty cells. Passing occurs when a player can't make a valid move. The phase concludes when there's no more space on the board. Unplaced pieces are aligned, forming a line, which holds significance in the scoring phase. The player who passed first initiates the scoring phase.

#### Scoring Phase

##### Removing Pieces

Both players start with 0 points, removing pieces in turns. The removed piece must be as large as or larger than the previous one by the same player. A piece can't be removed if it's under a score counter.

##### Scoring

Scoring follows a specific method:

- Calculate the remaining pieces in the line where a piece was removed for the base score.
- Multiply the base score by the value of the removed piece to get the multiplied score.
- Each score counter on the line doubles the final score.

##### Moving the Score Counter:
After scoring, players can adjust their score counter, moving it from 0 to the final score, ensuring no overlap with another counter.

The game's objective is to reach over 100 points. To break a tie, the winner is the player with the highest score. To break a tie again, the winner is the player with the longest line of unplaced pieces. To break the final tie, the winner is the black player. This Prolog implementation offers a captivating and strategic gaming experience, making *Isaac* a game of skill and tactics.

### Links:

- [Game Rules](https://www.iggamecenter.com/en/rules/isaac)
- [Youtube Video](http://www.youtube.com/watch?v=MwotyOed-Sw)
  

## Game Logic

### Internal Game State Representation

The Game State is represented as a state machine:

- Starts in `start` state;
- after choosing option in menu goes for `both_players_add_pieces` state;
- when one player cannot add pieces enters in `one_player_add_pieces` state;
- Enters second phase with `both_players_remove_pieces` state when no more pieces can be added;
- When a player passes, state updates to `one_player_remove_pieces`;
- Ends the game with `end_game` when a player reaches 100 points or both players pass.

The Player is represented with a double quoted string: "W" or "B". It is updated depending on the game state. If only one player can do moves, the player is kept. Else, it is updated from "W" to "B" and vice-versa.

If the player is a robot, we created a predicate that is called when moving: `piece_robot(Player, Difficulty)`.

There isn't such a board in our game. Instead we have predicates named `piece_position(Piece, Direction, Position)`, `sc(Player, Position)` and  `bpr(Player, Size)`, representing the positions of the pieces, the current score counters and the biggest removed piece of each player. Those are updated if needed, like the score counter predicate that is updated at each move in the states `both_players_remove_pieces` and `one_player_remove_pieces`.

For the pieces, including the `piece_position` predicate, we also have `piece(ID)`, in order to be able to identify each piece, `number_of_pieces(Piece_Size, Number_of_Pieces)` that tells us how many pieces exist from a certain size, `size_value(Size, Value)` that returns the value of a certain size, `piece_owner(Piece. Player)`, that checks if the Piece owner is the Player, and the `piece_size(Piece, Size)`,  that gives the size of a certain Piece. Using this we can get all the information about a piece.

### Game State Visualization

- Describe the implementation of the game state display predicate, which should be named `display_game(+GameState)`. Include information about the menu system, user interaction, and input validation. Highlight any appealing and intuitive visualizations.

### Move Validation and Execution

- Describe how plays are validated and executed to obtain a new game state. Explain the move predicate, which should be named `move(+GameState, +Move, -NewGameState)`.

### List of Valid Moves

- Explain how to obtain a list of possible moves using the `valid_moves` predicate. The predicate should be named `valid_moves(+GameState, +Player, -ListOfMoves)`.

### End of Game

- Describe the verification of the end of the game and the identification of the winner. The `game_over` predicate should be named `game_over(+GameState, -Winner)`.

### Game State Evaluation

- Explain how the game state is evaluated using the `value` predicate, which should be named `value(+GameState, +Player, -Value)`.


