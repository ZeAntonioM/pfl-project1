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

The game state is displayed in the console. The board is represented by a 10x10 matrix, where each cell represented a cell from 0 to 99. The pieces are represented in the board by arrows (representing their direction), the owner of the piece and the size of the piece, and the score counters are represented with a text "Black Player SC: "(or Write, depending on the player) and it's position. 

![Image with the board and it's representation](/images/Board.png)

The `display(+GameState, +Player)` predicate is called in the play predicate, and it calls the predicates that draws the board writes the Score Counters and writes whose turn it is.

```prolog

    display(_,Player):-
        draw_board(Player),
        sc("W", SCW),
        sc("B", SCB),
        draw_SC(Player, SCB,SCW),
        player_to_move(Player).

```

To validate inputs and moves we created the file `validators-pl`. In there, the inputs are validated depending on what we are checking: 

- `validate_size(+Size)` checks if the size is between 3 and 7
- `validate_direction(+Direction)` checks if the direction is one of the four possible directions
- `validate_position(+Position)` checks if the position is between 0 and 99
- `valid_piece(+Player, +Size, -Piece)` returns a valid piece for the player and size if exists
- `valid_position(+Size, +Position, +Direction)` checks if the position is valid for the size and direction
- `validate_menu_choice(+Choice)` checks if the choice is between 1 and 10
- `validate_piece_to_remove(+Piece)` checks if the piece is on the board
- `can_remove_piece(+Player, +Piece)` checks if the player can remove the piece

This predicates are usually called in the `move` predicate.

The menu has 10 options: 

![](/images/Menu.png)

### Move Validation and Execution

- Describe how plays are validated and executed to obtain a new game state. Explain the move predicate, which should be named `move(+GameState, +Move, -NewGameState)`.

### List of Valid Moves

Each time a move ends, it is necessary to check if there are any valid moves left for the next player. This is done by the `valid_moves` predicate, which is named `valid_moves(+GameState, +Player, -ListOfMoves)`.
If the Game state is on the second phase, it generates a list of all the pieces that can be removed by the player. If the Game state is on the first phase, it generates a list of all the possible Position-Direction-Size that the player can use to add a piece.

```prolog
valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        can_remove_pieces(Player,ListOfMoves ).
      
valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        can_place_pieces(Player,ListOfMoves ).  
```

The `can_remove_pieces` predicate uses a setof to get all the pieces that are placed on the board, are owned by the Player and can be removed, that is, there are no Score Counters above them and they are the biggest piece once removed by the Player. 

The `can_place_pieces` predicate uses a findall to get all the possible Position-Direction-Size that the player can use to add a piece. It uses the `valid_position` predicate to check if the position is valid for the size and direction and `valid_piece` to check if the piece is valid for the player and size. 

```prolog
can_place_pieces(Player, Pieces):-
        findall(Size-Direction-Position,can_place_piece(Player,Position,Size,Direction), Pieces ).

can_place_piece(Player,Position,Size,Direction):-
        can_place_piece(Player, Position, Size,Direction, 3).

can_place_piece(Player,Position,Acc,Direction, Acc):-
        valid_position(Acc,Position,Direction),
        valid_piece(Player, Acc,_Piece).

can_place_piece(Player,Position,Size,Direction, Acc):-
        validate_size(Acc),
        Acc2 is Acc +1,
        can_place_piece(Player,Position,Size,Direction, Acc2).
```


### End of Game

When the game state is updated, we check if the game is already over. This is done by the `game_over` predicate, which is named `game_over(+GameState, -Winner)`. It is impossible to draw in this game, so the only possible winners are "W" and "B". The sequence of break ties for winning are:

- The player who reaches 100 points first wins;
  - We check if there is a player whose score counter as reached 100 points.
- The player with the highest score wins;
  - We check both of the players score counters and see which one is higher.
- The player with the longest line of unplaced pieces wins;
  - We get the number of pieces remaining at the end of the first phase for both players, align them by players, and see which line is bigger.
- The player who started the game as black wins.

### Game State Evaluation

- Explain how the game state is evaluated using the `value` predicate, which should be named `value(+GameState, +Player, -Value)`.


