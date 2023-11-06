:-ensure_loaded('game_states.pl').
:-ensure_loaded('logic.pl').

%make_best_move(+GameState,+Player,-BestMove, +N,-V)
%given a gamestate a player and the number of iteration gives the best move and its value
%If gamestate is a removing phase the Best move is a pair containing the Piece to remove and the points to score
make_best_move(GameState,Player,BestMove-Points_, N,V):-
     (GameState = both_players_remove_pieces ;
     GameState = one_player_remove_pieces),
    valid_moves(GameState,Player,Moves),
    length(Moves,Size),
    Size >0,!,
    value(GameState, Player, Val),!,
    ((Val>=100)->
     V is 100;
    valid_moves(both_players_remove_pieces,Player,Moves),!,
    findall(Value-Points-Move,  (
            member(Move,Moves),
            simulate_n_remove(Player,Move,Points,Value,N)                                                           
), List ),!,
     sort(List, Rev),
     reverse_list(Rev,[V-Points_-BestMove|_])       )  ,!
.

make_best_move(_,_,_, _,0). % If it fails the value is 0

%apply_move(+GameState,+Player, +Move,+Positions) 
%It applies the move from the GameState and the Player
%If the GameState is a remove state then it removes a piece and updates both the Score Pointer and the
%Biggest Piece Removed
apply_move(GameState,Player, Piece-Points,Positions) :-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        sc(Player,SC),
        setof(Position, piece_position(Piece,_,Position),Positions),
        piece_position(Piece,Direction,Position),
        remove_piece(Piece),!,
        piece_owner(Piece, Player),
        bpr(Player,BPR),
        update_biggest_piece(Player,Piece,BPR),!,
        calculate_points( Piece, Position, Direction, Points),!,
        score_points(Player, SC, Points),!.

%reverse_move(+GameState,+Player,+Move,+SC, +BPR)
% It reverses a Move
%If the GameState is a remove state it adds a piece back
reverse_move(GameState,Player, Piece,Direction,Positions,SC, BPR):-
        (GameState = both_players_remove_pieces ;
        GameState = one_player_remove_pieces),
        retractall(bpr(Player,_)),
        asserta(bpr(Player,BPR)),
        retractall(sc(Player,_)),
        asserta(sc(Player,SC)),!,
        readd_pieces(Piece,Direction,Positions),!.

%readd_pieces(+Piece, +Direction, +Positions)
%It readds a piece to the board
readd_pieces(_, _, []) :- !.  % Base case, do nothing when the list is empty.
readd_pieces(Piece, Direction, [Head | Rest]) :-
    asserta(piece_position(Piece, Direction, Head)),
    readd_pieces(Piece, Direction, Rest).


%simulate_n_remove(+Player,+Move,+Points,+Value,+N)
% It simulates all possible removes with N plays and returns the best remove
% If N is 0 then it only simulates one remove, otherwise it simulates N removes
simulate_n_remove(Player,Move,Points,Value,0):-
        sc(Player,SC),
        bpr(Player,BPR),
        piece_position(Move,Direction,_Position),!,
        apply_move(GameState,Player,Move-Points,Positions),!,
        (value(GameState, Player, Value); Value is 0),!,
        reverse_move(GameState,Player, Move,Direction,Positions,SC, BPR),!.

simulate_n_remove(Player,Move,Points,Value,N):-
        N2 is N -1,
        sc(Player,SC),
        bpr(Player,BPR),
        piece_position(Move,Direction,_Position),!,
        apply_move(GameState,Player,Move-Points,Positions),!,
        ((value(GameState, Player, Value1),
           make_best_move(GameState,Player,_, N2,Value2),
           Value is (N*Value1+Value2); Value is 0 )),
        reverse_move(GameState,Player, Move,Direction,Positions,SC, BPR),!.

simulate_n_remove(_, _, 0). % If an error occurs the value is 0

%choose_piece(+Player, +robot_type,-Move)
%Gets a Piece to add
choose_piece(Player,hard,Piece-Direction-Position):-
        valid_moves(both_players_add_pieces,Player,[Piece-Direction-Position | _]).

