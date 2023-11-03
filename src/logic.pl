:- use_module(library(clpfd)).
:- use_module(library(between)).
:- ensure_loaded('pieces.pl').
:- ensure_loaded('io.pl').


% Changes the player
change_player("W", "B").

change_player("B", "W").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% 1st PHASE OF THE GAME %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

can_place_piece(Player,P,Size,Direction):-
        can_place_piece(Player, P, Size,Direction, 3).

can_place_piece(Player,P,Acc,Direction, Acc):-
        valid_position(Acc,P,Direction),
        valid_piece(Player, Acc,_Piece).

can_place_piece(Player,P,Size,Direction, Acc):-
        validate_size(Acc),
        Acc2 is Acc +1,
        can_place_piece(Player,P,Size,Direction, Acc2).

populate(Pos1, Pos2):-
        assertz((piece_position(_,_,_):-fail)),
        Pos1<Pos2,
        asserta(piece_position(1,h,Pos1)),
        Pos3 is Pos1 +1,
        populate(Pos3,Pos2).

populate(Pos1,Pos1).

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
get_line_values(h, [Position|T], Values) :-
    Line is Position div 10,
    findall(V, between(0+(1*Line), 9+(1*Line), V), Values).

get_line_values(v, [Position|T], Values) :-
    Column is Position mod 10,
    append([], [Column, Column+10, Column+20, Column+30, Column+40, Column+50, Column+60, Column+70, Column+80, Column+90], Values).


% Checks the number of pieces in the line
pieces_in_line(Values, Pieces) :-
    setof(Id, Direction^Position^(piece_position(Id,Direction,Position), member(Position, Values)),Res),
    length(Res, Pieces).
    
    
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
    
% Scores the points
score_points(Player, SCB, SCW, Points) :-
    Player == 'B',
    SCB1 is SCB + PointsToScore,
    SCB is SCB1.

score_points(Player, PointsToScore, SCB, SCW, Points) :-
    Player == 'W',
    SCW1 is SCW + PointsToScore,
    SCW is SCW1.



