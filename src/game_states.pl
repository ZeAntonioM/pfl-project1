:-ensure_loaded('draw.pl').
:-ensure_loaded('io.pl').
:-ensure_loaded('utils.pl').
:-ensure_loaded('ai.pl').

%%%%%%%% Display %%%%%%%%%%%%
% display(+GameState, +Player) displays the Board and the current player
% if the game is in the start state, it does nothing.
display(start,_).

% If the player is a robot, it does nothing
display(_,Player):-player_robot(Player,_),!.

% else it displays the board, the current player and the score counters.
display(_,Player):-
        draw_board(Player),
        sc("W", SCW),
        sc("B", SCB),
        draw_SC(Player, SCB,SCW),
        player_to_move(Player),!.


%%%%%%%% Ask Move %%%%%%%%%%%
% ask_for_move(+GameState, +Player, +Piece-Direction-Position) asks the user for a move
% if the game is in the start state, it does nothing.
ask_for_move(start,Player,Player).

% if the game is in the first move state and the player is a easy robot, it takes  a piece of a random valid move to add
ask_for_move(GameState, Player, Piece-Direction-Position):-
        player_robot(Player, easy),
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        piece_to_add_easy_ia(Player, Piece, Direction, Position).

% if the game is in the second move state and the player is a easy robot, it takes a piece of a random valid move  to removes
ask_for_move(GameState, Player, Piece-Direction-Position):-
        player_robot(Player, easy),
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        piece_to_remove_easy_ia(Player, Piece, Direction, Position).

% if the game is in the first move state and the player is a hard robot, it takes the best piece of  valid move  to add
ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        player_robot(Player,hard),
        write(Player),nl,
        piece_to_add_hard_ai(Player,Piece-_-Direction-Position),
        write(Piece),!.

% if the game is in the second move state and the player is a hard robot, it takes a the best piece of  valid move  to removes
ask_for_move(GameState,Player,Piece-Points):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        player_robot(Player,hard),
        make_best_move(GameState,Player,Piece-Points, 3,_).

% if the game is in the first move state, and the player is a human, it asks for a piece to add
ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        ask_for_piece_to_add(Player, Piece, Direction,Position).


% if the game is in the second move state, and the player is a human, it asks for a piece to remove
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
         state_machine(GameState, Player, NewGameState).

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
        (player_robot(Player, _), points_easy_ia(Points, PointsToScore),!; get_points_to_score(Points, PointsToScore),!),
        score_points(Player, SC, PointsToScore),!,
        state_machine(GameState, Player, NewGameState).

move(GameState, Piece-PointsToScore, NewGameState):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        piece_owner(Piece, Player),
        player_robot(Player,_),
        remove_piece(Piece),
         sc(Player,SC),
        bpr(Player,BPR),
        update_biggest_piece(Player,Piece,BPR),
        score_points(Player, SC, PointsToScore),!,
        state_machine(GameState, Player, NewGameState).
         


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
state_machine(start, Player, both_players_add_pieces):-
         change_player(Player, Next_player),
        valid_moves(both_players_add_pieces, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0.

% updates the start of the second phase state to both players remove pieces.
% stores the lenght of the line formed by the pieces that were left out of the board
state_machine(second_phase_start, _, both_players_remove_pieces):- 
        length_remaining_pieces("B", SizeB),
        length_remaining_pieces("W", SizeW),
        asserta(remaining_pieces("B", SizeB)),
        asserta(remaining_pieces("W", SizeW)),!.


% if both players can add or remove pieces, it gets the next player can do a valid move and updates the game state.
state_machine(GameState, Player, GameState):-

        (GameState = both_players_add_pieces;
         GameState = both_players_remove_pieces
            ),
        change_player(Player, Next_player),
        valid_moves(GameState, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.

% if only one player can add or remove pieces, it checks if the player can do a valid move and updates the game state.
state_machine(GameState, Player, GameState):-
        (GameState = one_player_add_pieces;
         GameState = one_player_remove_pieces),
        valid_moves(GameState, Player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.


% updates the start state to both players add pieces.
state_machine(start, Player, NewGameState):- !,state_machine(both_players_add_pieces, Player,NewGameState).

% updates the both players add pieces state to one player add pieces.
state_machine(both_players_add_pieces, Player, NewGameState):- !,state_machine(one_player_add_pieces, Player,NewGameState).

% updates the one player add pieces state to the start of the second phase.
state_machine(one_player_add_pieces, Player, NewGameState):- !,state_machine(second_phase_start, Player,NewGameState).

% updates the both players remove pieces state to one player remove pieces.
state_machine(both_players_remove_pieces, Player, NewGameState):- !,state_machine(one_player_remove_pieces, Player,NewGameState).


% updates the one player remove pieces state to end game.
state_machine(one_player_remove_pieces, _Player, end_game).


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

%%%%%%%%%% value %%%%%%%%%%%%%%


%value(+GameState, +Player, -Value)
% Evaluates how is th current GameState for the player
%Val goes from 0 to 100 in removing phase

% If while both players are removing a piece and the Player has 100 then he won the game
% value is 100
value(both_players_remove_pieces, Player, 100):-
        sc(Player,SC),
        SC >= 100,!.

% If while both players are removing a piece and the opposing Player has 100 then we loss the game
%Value is -100
value(both_players_remove_pieces, Player, Val):-
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        (SC2 >= 100),
        Val is (-100).

%If while both players are removing a piece the player cannot play anymore then value is 0
%If both players can remove a pieces and nobody won then value is their diferrence of points
value(both_players_remove_pieces, Player, Val):-
        valid_moves(both_players_remove_pieces, Player, ListOfMoves),
        length(ListOfMoves,Size),!,
        (Size == 0->
         (Val is 0);
         (sc(Player,SC),
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        Val is (SC - SC2))).

%If only onle player is removing pieces then he wons if he has more poins than the opponent
% value is 100
value(one_player_remove_pieces, Player, 100):-
        sc(Player,SC),
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        SC >SC2,!.

%If only onle player is removing pieces and can still remove more pieces then Val is thei difference of points
value(one_player_remove_pieces, Player, Diff):-
        valid_moves(one_player_remove_pieces, Player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,
        sc(Player,SC),
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        Diff is (SC - SC2),!.

%None of the player cann add pieces anymore
value(one_player_remove_pieces, _Player, 0).

% In adding phase the Value is obtain for the number of possible placements
value(GameState, Player, Val):-
         (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        valid_moves(GameState,Player,ListOfMoves),
        length(ListOfMoves,Val),!.
        





