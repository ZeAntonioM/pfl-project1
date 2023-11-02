:- use_module(library(clpfd)).
:- use_module(library(between)).
:- ensure_loaded('pieces.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% 1st PHASE OF THE GAME %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

can_place_piece(Player):-

.


add_piece(_,0,_,_).

add_piece(Piece,Size,Direction, Position):-
    (Direction = l;
        Direction = r),
    asserta(piece_position(Piece, h,Position)),
    next_position(Position,Direction,Next_position),
    Size2 is Size -1,
    add_piece(Piece,Size2,Direction, Next_position).

add_piece(Piece,Size,Direction, Position):-
    (Direction = u;
        Direction = d),
    asserta(piece_position(Piece, v,Position)),
    next_position(Position,Direction,Next_position),
    Size2 is Size -1,
    add_piece(Piece,Size2,Direction, Next_position).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% 2nd PHASE OF THE GAME %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Move in second phase. Checks if it's possible to remove a piece from the board, and if so, removes it. After that, calculates the points and finishes the move.
move2P(Player, Piece_id, SCB, SCW) :-
    findall(Position, piece_position(Piece_id, _, Position),  Positions),
    piece_position(Piece_id, Direction, _),
    check_move(Player, Piece_id, Positions, SCB, SCW),
    remove_piece(Piece_id),
    calculate_points(Player, Piece_id, Direction, Positions, SCB, SCW, Points),
    finish_move(Player, SCB, SCW, Piece_id, Points).

% Checks if the move is valid
check_move(Player, Piece_id, Positions, SCB, SCW) :-
    piece_owner(Player, Piece_id), %Checks if the piece belongs to the player
    check_valid_remove(Positions, SCB, SCW).    %Checks if there is a Score Counter in the piece's positions

check_valid_remove([], _, _).
check_valid_remove([Position|T], SCB, SCW) :-
    check_valid_remove(T, SCB, SCW),
    Position \= SCB,
    Position \= SCW.

% removes the piece from the board
remove_piece(Piece_id) :-
    retractall(piece_position(Piece_id, _, _)).

% Calculates the points of the move
calculate_points(Player, Piece_id, Direction, Positions, SCB, SCW, Points) :-
    get_line_values(Direction, Positions, Values),
    pieces_in_line(Values, Pieces),
    sc_in_line(Values, SCB, SCW, SC),
    piece_value(Piece_id, Value),
    Points is (Pieces * Value)*SC.

% Gets the values of the line
get_line_values(Direction, [Position|T], Values) :-
    Direction is 'H',
    Line is Position div 10,
    findall(V, between(0+(1*Line), 9+(1*Line), V), Values).

get_line_values(Direction, [Position|T], Values) :-
    Direction is 'V',
    Column is Position mod 10,
    append([], [Column, Column+10, Column+20, Column+30, Column+40, Column+50, Column+60, Column+70, Column+80, Column+90], Values).


% Checks the number of pieces in the line
pieces_in_line([], 0).
pieces_in_line([Value|T], Pieces) :-
    piece_position(_, _, Value),
    Pieces is Pieces1 + 1,
    pieces_in_line(T, Pieces1).
    
pieces_in_line([Value|T], Pieces) :-
    pieces_in_line(T, Pieces).
    
% Checks the number of score counters in the line
sc_in_line([], _, _, 0).
sc_in_line([Value|T], SCB, SCW, SC) :-
    Value == SCB,
    SC is SC1 + 1,
    sc_in_line(T, SCB, SCW, SC1).

sc_in_line([Value|T], SCB, SCW, SC) :-
    Value == SCW,
    SC is SC1 + 1,
    sc_in_line(T, SCB, SCW, SC1).

sc_in_line([Value|T], SCB, SCW, SC) :-
    sc_in_line(T, SCB, SCW, SC).


% Finishes the move
finish_move(Player, SCB, SCW, Piece_id, Points) :-
    get_points_to_score(Points, PointsToScore),
    score_points(Player, SCB, SCW, Points).
    

% Gets the points to score
get_points_to_score(Points, PointsToScore) :-
    write('You have removed a piece from the board and can score from 1 to '), write(Points), write(' points.'), nl,
    write('Choose the number of points you want to score: '), nl,
    read(PointsToScore),
    PointsToScore > 0,
    PointsToScore =< Points.

get_points_to_score(Points, PointsToScore) :-
    write('Invalid number of points. Try again.'), nl,
    get_points_to_score(Points, PointsToScore).

% Scores the points
score_points(Player, SCB, SCW, Points) :-
    Player == 'B',
    SCB1 is SCB + PointsToScore,
    SCB is SCB1.

score_points(Player, PointsToScore, SCB, SCW, Points) :-
    Player == 'W',
    SCW1 is SCW + PointsToScore,
    SCW is SCW1.

% Changes the player
change_player(Player, NewPlayer) :-
    Player == 'W',
    NewPlayer is 'B'.

change_player(Player, NewPlayer) :-
    Player == 'B',
    NewPlayer is 'W'.


