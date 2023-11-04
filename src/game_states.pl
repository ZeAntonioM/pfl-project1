:-ensure_loaded('draw.pl').
:-ensure_loaded('io.pl').

%%%%%%%% display %%%%%%%%%%%%

display(start,_).
display(_,Player):-
        draw_board(Player),
        sc("W", SCW),
        sc("B", SCB),
        draw_SC(Player, SCB,SCW),
        player_to_move(Player).


%%%%%%%% ask move %%%%%%%%%%%

ask_for_move(start,Player,Player).
ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        ask_for_piece_to_add(Player, Piece, Direction,Position).

ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        ask_for_piece_to_remove(Player, Piece, Direction,Position).

%%%%%%%%% Move %%%%%%%%%%

move(start, Player,NewGameState):-value(start,Player, NewGameState ).
move(GameState, Piece-Direction-Position, NewGameState):-
         (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
         piece_size(Piece, Size),
         add_piece(Piece, Size, Direction, Position),
         piece_owner(Piece, Player),
         value(GameState, Player, NewGameState).

move(GameState, Piece-Direction-Position, NewGameState):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        remove_piece(Piece),
        piece_owner(Piece, Player),
         sc(Player,SC),
        bpr(Player,BPR),
        update_biggest_piece(Player,Piece,BPR),
        calculate_points( Piece, Position, Direction, Points),!,
        get_points_to_score(Points, PointsToScore),!,
        score_points(Player, SC, PointsToScore),!,
        value(GameState, Player, NewGameState).
         


%%%%%%% change player $$$$$$$$$$$$$

change_player(GameState,"W", "B"):-
        (GameState = both_players_add_pieces;
        GameState = both_players_remove_pieces
        ).

change_player(GameState,"B", "W"):-
        (GameState = both_players_add_pieces;
        GameState = both_players_remove_pieces
        ).

change_player(_,Player, Player).

%%%%%%%% Value %%%%%%%%%%%

value(start, Player, both_players_add_pieces):-
         change_player(Player, Next_player),
        valid_moves(both_players_add_pieces, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0.

value(GameState, Player, GameState):-
        (GameState = both_players_add_pieces;
         GameState = both_players_remove_pieces
            ),
        change_player(Player, Next_player),
        valid_moves(GameState, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.

value(GameState, Player, GameState):-
        (GameState = one_player_add_pieces;
         GameState = one_player_remove_pieces),
        valid_moves(GameState, Player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.



value(start, Player, NewGameState):- !,value(both_players_add_pieces, Player,NewGameState).
value(both_players_add_pieces, Player, NewGameState):- !,value(one_player_add_pieces, Player,NewGameState).
value(one_player_add_pieces, Player, NewGameState):- !,value(both_players_remove_pieces, Player,NewGameState).
value(both_players_remove_pieces, Player, NewGameState):- !,value(one_player_remove_pieces, Player,NewGameState).
value(one_player_remove_pieces, _Player, end_game).


%%%%%% valid Moves %%%%%

valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        can_remove_pieces(Player,ListOfMoves ).
      
valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        can_place_pieces(Player,ListOfMoves ).  

        