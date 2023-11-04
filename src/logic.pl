:- use_module(library(clpfd)).
:- use_module(library(between)).
:- ensure_loaded('pieces.pl').
:- ensure_loaded('io.pl').
:- ensure_loaded('utils.pl').


% Changes the player from W to B
change_player("W", "B").

% Changes the player from B to W
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
    setof(Piece, Player^Direction^Position^(piece_position(Piece, Direction, Position),piece_owner(Piece, Player)), Pieces),
    can_remove_pieces(Player, Pieces, BiggestPieceB, BiggestPieceW, SCB, SCW).

can_remove_pieces(_Player, [], _BiggestPieceB, _BiggestPieceW, _SCB, _SCW):-fail.
can_remove_pieces("B", [Piece|_T], BiggestPieceB, BiggestPieceW, SCB, SCW) :-
    findall(Position, piece_position(Piece, _, Position),  Positions),
    can_remove_piece("B", Piece, BiggestPieceB, BiggestPieceW, Positions, SCB, SCW).

can_remove_pieces("W", [Piece|_T], BiggestPieceB, BiggestPieceW, SCB, SCW) :-
    findall(Position, piece_position(Piece, _, Position),  Positions),
    can_remove_piece("W", Piece, BiggestPieceB, BiggestPieceW, Positions, SCB, SCW).

can_remove_pieces(Player, [_Piece|T], BiggestPieceB, BiggestPieceW, SCB, SCW) :-
    can_remove_pieces(Player, T, BiggestPieceB, BiggestPieceW, SCB, SCW),!.


% removes the piece from the board
remove_piece(Piece) :-
    retractall(piece_position(Piece, _, _)).

% Calculates the points of the move
calculate_points( Piece, Position, Direction, SCB, SCW, Points) :-
    get_line_values(Direction, Position, Values),!,
    pieces_in_line(Values, Pieces),
    sc_in_line(Values, SCB, SCW, SC),
    piece_value(Piece, Value),
    multiply_points(Pieces, Value, SC, Points).


% Gets the values of the line
get_line_values(h, Position, Values) :-
    Line is Position div 10,
    Start is (10*Line + 0),
    End is (10*Line +9),
    findall(V, between(Start,End , V), Values)
    .

get_line_values(v, Position, Values) :-
    Column is Position mod 10,
    generate_columns(Column, Values).

% Checks the number of pieces in the line
pieces_in_line(Values, Pieces) :-
    setof(Id, Direction^Position^(piece_position(Id,Direction,Position), member(Position, Values)),Res),
    length(Res, Pieces).

pieces_in_line(_Values, 0).
    
    
% Checks the number of score counters in the line
sc_in_line([], _, _, 0).
sc_in_line([Value|T], SCB, SCW, SC) :-
    Value == SCB,
    sc_in_line(T, SCB, SCW, SC1),
    SC is SC1 + 1.

sc_in_line([Value|T], SCB, SCW, SC) :-
    NEW_SCW is 99 - SCW,
    Value == NEW_SCW,
    sc_in_line(T, SCB, SCW, SC1),
    SC is SC1 + 1.

sc_in_line([_Value|T], SCB, SCW, SC) :-
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
    NewSCW is SCW + PointsToScore,
    NewSCB is SCB.

update_biggest_piece("W", Piece, BPRB, BPRW,BPRB, Size):- piece_size(Piece,Size),between(BPRW,7, Size) .
update_biggest_piece("B", Piece, BPRB, BPRW,Size, BPRW):-piece_size(Piece,Size),between(BPRB,7, Size).
update_biggest_piece(_, _, BPRB, BPRW,BPRB, BPRW).
update_biggest_piece(Piece, BPR, Size):-piece_size(Piece,Size), between(BPR,7, Size).
update_biggest_piece(_, BPR, BPR).

populate:-
              add_piece(15,7,u,0),
              add_piece(14,6,u,1),
              add_piece(13,6,u,2),
              add_piece(12,5,u,3),
              add_piece(11,5,u,4),
              add_piece(10,5,u,5),
              add_piece(9,4,u,6),
              add_piece(8,4,u,7),
              add_piece(7,4,u,8),
              add_piece(1,3,u,9),
              add_piece(2,3,d,68),
              add_piece(3,3,d,69),

              %add_piece(30,7,d,99),
              %add_piece(29,6,d,98),
              %add_piece(28,6,d,97),
              add_piece(18,3,r,87),
              add_piece(19,3,r,77),
              add_piece(17,3,r,97),
              add_piece(20, 3, d,67),
              add_piece(27,5,d,96),
              add_piece(26,5,d,95),
              add_piece(25,5,d,94),
              add_piece(24,4,d,93),
              add_piece(23,4,d,92),
              add_piece(22,4,d,91),
              add_piece(16,3,d,90)
              .
