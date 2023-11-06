:-ensure_loaded('draw.pl').
:-ensure_loaded('io.pl').
:-ensure_loaded('utils.pl').
:-ensure_loaded('ai.pl').
%%%%%%%% display %%%%%%%%%%%%

display(start,_):-!.
display(_,Player):-player_robot(Player,_),!.
display(_,Player):-
        draw_board(Player),
        sc("W", SCW),
        sc("B", SCB),
        draw_SC(Player, SCB,SCW),
        player_to_move(Player),!.


%%%%%%%% ask move %%%%%%%%%%%

ask_for_move(start,Player,Player).
ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        
        player_robot(Player,Type),
        write(Player),nl,
        choose_piece(Player,Type,Piece-_-Direction-Position),
        write(Piece),!.

ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        ask_for_piece_to_add(Player, Piece, Direction,Position).

ask_for_move(GameState,Player,Piece-Points):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        write(Player),
        player_robot(Player,_),
        make_best_move(GameState,Player,Piece-Points, 3,_).

ask_for_move(GameState,Player,Piece-Direction-Position):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        ask_for_piece_to_remove(Player, Piece, Direction,Position).



%%%%%%%%% Move %%%%%%%%%%

move(start, Player,NewGameState):-state_machine(start,Player, NewGameState ).
move(GameState, Piece-Direction-Position, NewGameState):-
         (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
         piece_size(Piece, Size),
         write(Size),nl,
         write(Piece),nl,
         add_piece(Piece, Size, Direction, Position),
         piece_owner(Piece, Player),
         write(Player),nl,
         state_machine(GameState, Player, NewGameState).

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

state_machine(start, Player, both_players_add_pieces):-
         change_player(Player, Next_player),
        valid_moves(both_players_add_pieces, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0.

state_machine(GameState, Player, GameState):-
        (GameState = both_players_add_pieces;
         GameState = both_players_remove_pieces
            ),
        change_player(Player, Next_player),
        valid_moves(GameState, Next_player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.

state_machine(GameState, Player, GameState):-
        (GameState = one_player_add_pieces;
         GameState = one_player_remove_pieces),
        valid_moves(GameState, Player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,!.



state_machine(start, Player, NewGameState):- !,state_machine(both_players_add_pieces, Player,NewGameState).
state_machine(both_players_add_pieces, Player, NewGameState):- !,state_machine(one_player_add_pieces, Player,NewGameState).
state_machine(one_player_add_pieces, Player, NewGameState):- !,state_machine(both_players_remove_pieces, Player,NewGameState).
state_machine(both_players_remove_pieces, Player, NewGameState):- !,state_machine(one_player_remove_pieces, Player,NewGameState).
state_machine(one_player_remove_pieces, _Player, end_game).


%%%%%% valid Moves %%%%%

valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        can_remove_pieces(Player,ListOfMoves ).
      
valid_moves(GameState, Player, ListOfMoves):-
        (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        can_place_pieces(Player,ListOfMoves ).  

%%%%%%%%%% value %%%%%%%%%%%%%%


value(both_players_remove_pieces, Player, 100):-
        sc(Player,SC),
        SC >= 100,!.

value(both_players_remove_pieces, Player, Val):-
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        (SC2 >= 100),
        Val is 0
        .

value(both_players_remove_pieces, Player, Val):-
        valid_moves(both_players_remove_pieces, Player, ListOfMoves),
        length(ListOfMoves,Size),!,
        (Size == 0->
         (Val is 0);
         (
                sc(Player,SC),
                change_player(Player,Next_Player),
                sc(Next_Player,SC2),
                Val is (SC - SC2)
             )

         ).


value(one_player_remove_pieces, Player, 100):-
        sc(Player,SC),
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        SC >SC2,!.

value(one_player_remove_pieces, Player, Diff):-
        valid_moves(one_player_remove_pieces, Player, ListOfMoves),
        length(ListOfMoves,Size),
        Size>0,
        sc(Player,SC),
        change_player(Player,Next_Player),
        sc(Next_Player,SC2),
        Diff is (SC - SC2),!.

value(one_player_remove_pieces, _Player, 0).


value(GameState, Player, Size):-
         (GameState = both_players_add_pieces ;
        GameState = one_player_add_pieces),
        valid_moves(GameState,Player,ListOfMoves),
        length(ListOfMoves,Size),!.
        





