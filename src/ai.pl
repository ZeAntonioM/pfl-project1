:-ensure_loaded('game_states.pl').
:-ensure_loaded('logic.pl').
make_best_move(GameState,Player,BestMove-Points_, 0,V):-
    (GameState = both_players_remove_pieces ;
     GameState = one_player_remove_pieces),
    valid_moves(GameState,Player,Moves),!,
    length(Moves,Size),
    Size >0,
    write(Size),nl,
    value(GameState, Player, Val),
    write('Size'), write(Size),nl,
    sc(Player,SC),
    ((Val>=100) ->
     V is 100;
    bpr(Player,BPR),
    
    findall(Value-Move-Points-0,  ( piece_position(Move,Direction,_Position),
                                    member(Move,Moves),
                                   (apply_move(GameState,Player,Move-Points,Positions),
                                   (value(GameState, Player, Value)),
                                   reverse_move(GameState,Player, Move,Direction,Positions,SC, BPR))
                                                                
), List ),!,
     sort(List, Rev),
     reverse_list(Rev,[V-BestMove-Points_-_N|_])       )    ,!              
.

make_best_move(GameState,Player,BestMove-Points_, N,V):-
    N>0,
     (GameState = both_players_remove_pieces ;
     GameState = one_player_remove_pieces),
     N2 is (N-1),
    sc(Player,SC),
    valid_moves(GameState,Player,Moves),!,
    length(Moves,Size),
    Size >0,
    write('Size'), write(Size),nl,
    value(GameState, Player, Val),!,
    ((Val>=100)->
     V is 100;
    bpr(Player,BPR),
    valid_moves(both_players_remove_pieces,Player,Moves),!,
    findall(Value-Move-Points-N,  ( piece_position(Move,Direction,_Position),
                                    member(Move,Moves),
                                   apply_move(GameState,Player,Move-Points,Positions),
                                   (value(GameState, Player, Value1),
                                   make_best_move(GameState,Player,_, N2,Value2),
                                   Value is (Value1+Value2) ),
                                   reverse_move(GameState,Player, Move,Direction,Positions,SC, BPR)
                                                                
), List ),!,
     sort(List, Rev),
     write_list_(Rev),
     reverse_list(Rev,[V-BestMove-Points_-N|_])       )  ,!
.

make_best_move(_,_,_-_, _,0).


apply_move(GameState,Player, Piece-Points,Positions) :-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        sc(Player,SC),
        setof(Position, piece_position(Piece,_,Position),Positions),
        piece_position(Piece,Direction,Position),
        remove_piece(Piece),
        piece_owner(Piece, Player),
        bpr(Player,BPR),
        update_biggest_piece(Player,Piece,BPR),!,
        calculate_points( Piece, Position, Direction, Points),!,
        score_points(Player, SC, Points),!.


reverse_move(GameState,Player, Piece,Direction,Positions,SC, BPR):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        retractall(bpr(Player,_)),
        asserta(bpr(Player,BPR)),
        retractall(sc(Player,_)),
        asserta(sc(Player,SC)),!,
        readd_pieces(Piece,Direction,Positions),!.


readd_pieces(_, _, []) :- !.  % Base case, do nothing when the list is empty.
readd_pieces(Piece, Direction, [Head | Rest]) :-
    asserta(piece_position(Piece, Direction, Head)),
    readd_pieces(Piece, Direction, Rest).

choose_piece(Player,hard,Piece-Direction-Position):-
        valid_moves(both_players_add_pieces,Player,[Piece-Direction-Position | _]).

