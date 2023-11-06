:-ensure_loaded('draw.pl').
:-ensure_loaded('io.pl').

%%%%%%%% Display %%%%%%%%%%%%

% display(+GameState, +Player) displays the Board and the current player
% if the game is in the start state, it does nothing.
display(start,_).
% else it displays the board, the current player and the score counters.
display(_,Player):-
        draw_board(Player),
        sc("W", SCW),
        sc("B", SCB),
        draw_SC(Player, SCB,SCW),
        player_to_move(Player).

%%%%%%%% Ask Move %%%%%%%%%%%

% ask_for_move(+GameState, +Player, +Piece-Direction-Position) asks the user for a move
% if the game is in the start state, it does nothing.
ask_for_move(start,Player,Player).


% if the game is in the first move state and the player is a easy robot, it tooks adds a piece of a random valid move
ask_for_move(GameState, Player, Piece-Direction-Position):-
        player_robot(Player, easy),
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        piece_to_add_easy_ia(Player, Piece, Direction, Position).

% if the game is in the second move state and the player is a easy robot, it tooks removes a piece of a random valid move
ask_for_move(GameState, Player, Piece-Direction-Position):-
        player_robot(Player, easy),
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        piece_to_remove_easy_ia(Player, Piece, Direction, Position).

% if the game is in the first move state, it asks for a piece to add
ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        ask_for_piece_to_add(Player, Piece, Direction,Position).

% if the game is in the second move state, it asks for a piece to remove
ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        ask_for_piece_to_remove(Player, Piece, Direction,Position).

%%%%%%%%% Move %%%%%%%%%%

% move(+GameState, +Piece-Direction-Position, -NewGameState) makes the move
% if the game is in the start state, it updates the game state.
move(start, Player,NewGameState):-value(start,Player, NewGameState ).

% if the game is in the first move state, it adds the piece to the board and updates the game state.
move(GameState, Piece-Direction-Position, NewGameState):-
         (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
         piece_size(Piece, Size),
         add_piece(Piece, Size, Direction, Position),
         piece_owner(Piece, Player),
         value(GameState, Player, NewGameState).

% if the game is in the second move state, it removes the piece from the board, updates the score counters 
% and the biggest piece removed by the player and updates the game state.
move(GameState, Piece-Direction-Position, NewGameState):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        remove_piece(Piece),
        piece_owner(Piece, Player),
         sc(Player,SC),
        bpr(Player,BPR),
        update_biggest_piece(Player,Piece,BPR),
        calculate_points( Piece, Position, Direction, Points),!,
        (player_robot(Player, _), points_ia(Points, PointsToScore),!; get_points_to_score(Points, PointsToScore),!),
        score_points(Player, SC, PointsToScore),!,
        value(GameState, Player, NewGameState).


%%%%%%% Change Player $$$$$$$$$$$$$
% change_player(+GameState, +Player, -NewPlayer) changes the player
% if both player can add or remove pieces, it changes the player from W to B.
change_player(GameState,"W", "B"):-
        (GameState = both_players_add_pieces;
        GameState = both_players_remove_pieces
        ).

% if only one player can add or remove pieces, it changes the player from B to W.
change_player(GameState,"B", "W"):-
        (GameState = both_players_add_pieces;
        GameState = both_players_remove_pieces
        ).

% if only one player can add or remove pieces, it mantains the player.
change_player(_,Player, Player).

%%%%%%%% Value %%%%%%%%%%%

% value(+GameState, +Player, -NewGameState) updates the game state
% if the game is in the start state, it gets the next player and checks if the next player can add pieces. Finally, it returns the game state to the first phase.
value(start, Player, both_players_add_pieces):-
         change_player(Player, Next_player),
        valid_moves(both_players_add_pieces, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0.

% if both players can add or remove pieces, it gets the next player can do a valid move and updates the game state.
value(GameState, Player, GameState):-
        (GameState = both_players_add_pieces;
         GameState = both_players_remove_pieces
            ),
        change_player(Player, Next_player),
        valid_moves(GameState, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.

% if only one player can add or remove pieces, it checks if the player can do a valid move and updates the game state.
value(GameState, Player, GameState):-
        (GameState = one_player_add_pieces;
         GameState = one_player_remove_pieces),
        valid_moves(GameState, Player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.

% updates the start state to both players add pieces.
value(start, Player, NewGameState):- !,value(both_players_add_pieces, Player,NewGameState).

% updates the both players add pieces state to one player add pieces.
value(both_players_add_pieces, Player, NewGameState):- !,value(one_player_add_pieces, Player,NewGameState).

% updates the one player add pieces state to both players remove pieces.
value(one_player_add_pieces, Player, NewGameState):- !,value(both_players_remove_pieces, Player,NewGameState).

% updates the both players remove pieces state to one player remove pieces.
value(both_players_remove_pieces, Player, NewGameState):- !,value(one_player_remove_pieces, Player,NewGameState).

% updates the one player remove pieces state to end game.
value(one_player_remove_pieces, _Player, end_game).


%%%%%% Valid Moves %%%%%

%valid_moves(+GameState, +Player, -ListOfMoves) returns a list of valid moves
%if both players can remove pieces, it returns a list of valid moves to remove pieces.
valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        can_remove_pieces(Player,ListOfMoves ).
      
% if both players can add pieces, it returns a list of valid moves to add pieces.
valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        can_place_pieces(Player,ListOfMoves ).  

        