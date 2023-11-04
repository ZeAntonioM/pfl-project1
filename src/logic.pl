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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
populate(Pos1, Pos2):-
        assertz((piece_position(_,_,_):-fail)),
        Pos1<Pos2,
        asserta(piece_position(1,h,Pos1)),
        Pos3 is Pos1 +1,
        populate(Pos3,Pos2).

populate(Pos1,Pos1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

can_remove_pieces(Player, BiggestPieceB, BiggestPieceW, SCB, SCW):-
    setof(Piece, Direction^Position^(piece_position(Piece, Direction, Position), piece_owner(Piece, Player)), Pieces),
    can_remove_pieces(Player, Pieces, BiggestPieceB, BiggestPieceW), SCB, SCW.

can_remove_pieces(Player, [], BiggestPieceB, BiggestPieceW, SCB, SCW).
can_remove_pieces("B", [Piece|T], BiggestPieceB, BiggestPieceW) :-
    findall(Position, piece_position(Piece, _, Position),  Positions),
    can_remove_piece("B", Piece, BiggestPieceB, BiggestPieceW, Positions, SCB, SCW).

can_remove_pieces("W", [Piece|T], BiggestPieceB, BiggestPieceW, SCB, SCW) :-
    findall(Position, piece_position(Piece, _, Position),  Positions),
    can_remove_piece("W", Piece, BiggestPieceB, BiggestPieceW, Positions, SCB, SCW).

can_remove_pieces(Player, [Piece|T], BiggestPieceB, BiggestPieceW, SCB, SCW) :-
    can_remove_pieces(Player, T, BiggestPieceB, BiggestPieceW, SCB, SCW).


% removes the piece from the board
remove_piece(Piece) :-
    retractall(piece_position(Piece, _, _)).

% Calculates the points of the move
calculate_points(Player, Piece, Direction, Position, SCB, SCW, Points) :-
    get_line_values(Direction, Positions, Values),
    pieces_in_line(Values, Pieces),
    sc_in_line(Values, SCB, SCW, SC),
    piece_value(Piece, Value),
    multiply_points(Pieces, Value, SC, Points).

% Gets the values of the line
get_line_values(h, Position, Values) :-
    Line is Position div 10,
    findall(V, between(0+(10*Line), 9+(10*Line), V), Values).

get_line_values(v, Position, Values) :-
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
    sc_in_line(T, SCB, SCW, SC1),
    SC is SC1 + 1.

sc_in_line([Value|T], SCB, SCW, SC) :-
    Value == SCW,
    sc_in_line(T, SCB, SCW, SC1),
    SC is SC1 + 1.

sc_in_line([Value|T], SCB, SCW, SC) :-
    sc_in_line(T, SCB, SCW, SC).

multiply_points(Pieces, Value, 0, Points):-
    Points is Pieces * Value.

multiply_points(Pieces, Value, SC, Points) :-
    Points is Pieces * Value * 2 * SC.
    
% Scores the points
score_points("B", SCB, SCW, PointsToScore, NewSCB, NewSCW) :-
    NewSCB is SCB + PointsToScore,
    NewSCW is SCW.

score_points("W", SCB, SCW, PointsToScore, NewSCB, NewSCW) :-
    NewScw is SCW + PointsToScore,
    NewSCB is SCB.


loop2phase("B", SCB, SCW, BiggestPieceB, BiggestPieceW):-
    SCB < 100,
    phase2cycle("W", SCB, SCW, BiggestPieceB, BiggestPieceW).

loop2phase("B", SCB, SCW, BiggestPieceB, BiggestPieceW):-
    write('Player B Won! Congratulations!').

loop2phase("W", SCB, SCW, BiggestPieceB, BiggestPieceW):-
    SCW < 100,
    phase2cycle("B", SCB, SCW, BiggestPieceB, BiggestPieceW).

loop2phase("W", SCB, SCW, BiggestPieceB, BiggestPieceW):-
    write('Player W Won! Congratulations!').

check_continue_2_phase("B", SCB, SCW):-
    SCB >= 100.

check_continue_2_phase("W", SCB, SCW):-
    SCW >= 100.
