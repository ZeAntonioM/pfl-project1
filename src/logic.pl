:- use_module(library(clpfd)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).
:- ensure_loaded('pieces.pl').
:- ensure_loaded('io.pl').
:- ensure_loaded('utils.pl').

init_random_state:-
    now(Seed),
    setrand(Seed).

% Changes the player
change_player("W", "B").

change_player("B", "W").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% 1st PHASE OF THE GAME %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


can_place_pieces(Player, Pieces):-
        findall(Size-Direction-Position,can_place_piece(Player,Position,Size,Direction), Pieces ).

can_place_piece(Player,Position,Size,Direction):-
        can_place_piece(Player, Position, Size,Direction, 3).

can_place_piece(Player,Position,Acc,Direction, Acc):-
        valid_position(Acc,Position,Direction),
        valid_piece(Player, Acc,_Piece).

can_place_piece(Player,Position,Size,Direction, Acc):-
        validate_size(Acc),
        Acc2 is Acc +1,
        can_place_piece(Player,Position,Size,Direction, Acc2).
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

piece_to_add_easy_ia(Player, Piece, Direction, Position):-
        can_place_pieces(Player, Moves),
        random_member((Size-Direction-Position), Moves),
        %random_member((Size-Direction1-Position1), Moves),
        %convert_position(Player, Position1, Position),
        %convert_direction(Player, Direction1, Direction),
        valid_piece(Player, Size, Piece).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% 2nd PHASE OF THE GAME %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

can_remove_pieces(Player, Pieces):-
    setof(
             Piece,
             Player^Direction^Position^(
                                           piece_position(Piece, Direction, Position),
                                           piece_owner(Piece, Player),
                                           can_remove_piece(Player, Piece)
                                       ),
             Pieces
         )
    .

can_remove_pieces(_, []).

piece_to_remove_easy_ia(Player, Piece, Direction, Position):-
        can_remove_pieces(Player, Pieces),
        random_member(Piece, Pieces),
        piece_position(Piece, Direction, Position).

% removes the piece from the board
remove_piece(Piece) :-
    retractall(piece_position(Piece, _, _)).

% Calculates the points of the move
calculate_points( Piece, Position, Direction, Points) :-
     sc("W", SCW),
     sc("B", SCB),
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

points_ia(Points, PointsToScore):-
    findall(X, between(0,Points, X), Values),
    random_member(PointsToScore, Values).

multiply_points(Pieces, Value, 0, Points):-
    Points is Pieces * Value.

multiply_points(Pieces, Value, SC, Points) :-
    Points is Pieces * Value * 2 * SC.
    

score_points(Player , SC, PointsToScore) :-
     Points is SC + PointsToScore,
    retractall(sc(Player,_)),
    asserta(sc(Player,Points)).

update_biggest_piece(Player, Piece, BPR):-
        piece_size(Piece,Size),
        between(BPR,7, Size),
        retractall(bpr(Player,_)),
        asserta(bpr(Player,Size)).

length_remaining_pieces(Player, Length):-
        
        findall(Piece, (piece_position(Piece,_,_), piece_owner(Piece, Player), piece_size(Piece, 3)), N3),
        findall(Piece, (piece_position(Piece,_,_), piece_owner(Piece, Player), piece_size(Piece, 4)), N4),
        findall(Piece, (piece_position(Piece,_,_), piece_owner(Piece, Player), piece_size(Piece, 5)), N5),
        findall(Piece, (piece_position(Piece,_,_), piece_owner(Piece, Player), piece_size(Piece, 6)), N6),
        findall(Piece, (piece_position(Piece,_,_), piece_owner(Piece, Player), piece_size(Piece, 7)), N7),
        
        length(N3, L3),
        length(N4, L4),
        length(N5, L5),
        length(N6, L6),
        length(N7, L7),

        R3 is 15 - L3,
        R4 is 16 - L4,
        R5 is 15 - L5,
        R6 is 12 - L6,
        R7 is 7 - L7,

        Length is R3 + R4 + R5 + R6 + R7.

biggest_lenght(LengthB, LengthW, Player) :-
    LengthB > LengthW,
    Player is "B".

biggest_lenght(LengthB, LengthW, Player) :-
    LengthB < LengthW,
    Player is "W".

    