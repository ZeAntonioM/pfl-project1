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

% can_place_pieces(+Player, -Pieces) returns a list of pieces that the player can place
can_place_pieces(Player, Pieces):-
        findall(Size-Direction-Position,can_place_piece(Player,Position,Size,Direction), Pieces ).

% can_place_piece(+Player, +Position, +Size, +Direction) checks if the player can place a piece in the given position, size and direction
% calls an auxiliary predicate with the minimum size of the piece
can_place_piece(Player,Position,Size,Direction):-
        can_place_piece(Player, Position, Size,Direction, 3).

% can_place_piece(+Player, +Position, +Size, +Direction, +Acc) auxiliary predicate that checks if the player can place a piece in the given position, size and direction
% If Acc is equal to the size of the piece, just checks if the position is valid and if the piece exists
can_place_piece(Player,Position,Acc,Direction, Acc):-
        valid_position(Acc,Position,Direction),
        valid_piece(Player, Acc,_Piece).

% If Acc is not equal to the size of the piece, validates if Acc is a valid size and calls the predicate with Acc+1
can_place_piece(Player,Position,Size,Direction, Acc):-
        validate_size(Acc),
        Acc2 is Acc +1,
        can_place_piece(Player,Position,Size,Direction, Acc2).

% add_piece(+Piece, +Size, +Direction, +Position) adds a piece to the board
% if size is 0, just returns
add_piece(_,0,_,_).

% if direction is left or right, adds all the piece's positions to the board horizontally
add_piece(Piece,Size,Direction, Position):-
    (Direction = l;
        Direction = r),
    asserta(piece_position(Piece, h,Position)),
    next_position(Position,Direction,Next_position),
    Size2 is Size -1,
    add_piece(Piece,Size2,Direction, Next_position).

% if direction is up or down, adds all the piece's positions to the board vertically
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

% can_move_pieces(+Player, -Pieces) returns a list of pieces that the player can move
% gets all the pieces in board, checks if the player is the owner of the piece and if the piece can be removed
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

% if there is no piece to remove, returns an empty list
can_remove_pieces(_, []).

piece_to_remove_easy_ia(Player, Piece, Direction, Position):-
        can_remove_pieces(Player, Pieces),
        random_member(Piece, Pieces),
        piece_position(Piece, Direction, Position).

% remove_piece(+Piece) removes all the piece's positions from the board
remove_piece(Piece) :-
    retractall(piece_position(Piece, _, _)).

% calculate_points( +Piece, +Position, +Direction, -Points) calculates the points of a move
% gets the number of pieces in line with the removed piece, checks the number of score counters in the line and calculates the points
calculate_points( Piece, Position, Direction, Points) :-
     sc("W", SCW),
     sc("B", SCB),
    get_line_values(Direction, Position, Values),!,
    pieces_in_line(Values, Pieces),
    sc_in_line(Values, SCB, SCW, SC),
    piece_value(Piece, Value),
    multiply_points(Pieces, Value, SC, Points).


% get_line_values(+Direction, +Position, -Values) gets all the positions in the line of the given position
% if direction is horizontal, gets all the positions between the first and last position of the line
get_line_values(h, Position, Values) :-
    Line is Position div 10,
    Start is (10*Line + 0),
    End is (10*Line +9),
    findall(V, between(Start,End , V), Values).

% if direction is verticall, generates all the positions in the column of the given position
get_line_values(v, Position, Values) :-
    Column is Position mod 10,
    generate_columns(Column, Values).

% pieces_in_line(+Values, -Pieces) gets the number of pieces in the line
% gets all the pieces in the line and counts them
pieces_in_line(Values, Pieces) :-
    setof(Id, Direction^Position^(piece_position(Id,Direction,Position), member(Position, Values)),Res),
    length(Res, Pieces).

% if there is no piece in the line, returns 0
pieces_in_line(_Values, 0).
    
% sc_in_line(+Values, +SCB, +SCW, -SC) gets the number of score counters in the line
% if there are no more values in the list, returns 0
sc_in_line([], _, _, 0).

% if the value is equal to the score counter of the black player, increments the counter and calls the predicate with the rest of the list
sc_in_line([Value|T], SCB, SCW, SC) :-
    Value == SCB,
    sc_in_line(T, SCB, SCW, SC1),
    SC is SC1 + 1.

% if the value is equal to the score counter of the white player, converts the score counter's values, increments the counter and calls the predicate with the rest of the list
sc_in_line([Value|T], SCB, SCW, SC) :-
    NEW_SCW is 99 - SCW,
    Value == NEW_SCW,
    sc_in_line(T, SCB, SCW, SC1),
    SC is SC1 + 1.

% if the value is not equal to the score counter of the black or white player, calls the predicate with the rest of the list
sc_in_line([_Value|T], SCB, SCW, SC) :-
    sc_in_line(T, SCB, SCW, SC).

points_ia(Points, PointsToScore):-
    findall(X, between(0,Points, X), Values),
    random_member(PointsToScore, Values).

% multiply_points(+Pieces, +Value, +SC, -Points) calculates the points of a move
% if there are no score counters in the line, just multiplies the number of pieces by the value of the piece
multiply_points(Pieces, Value, 0, Points):-
    Points is Pieces * Value.

% if there are score counters in the line, multiplies the number of pieces by the value of the piece and multiplies the result by 2
multiply_points(Pieces, Value, SC, Points) :-
    Points is Pieces * Value * 2 * SC.
    
% score_points(+Player, +SC, +PointsToScore) updates the score of the player
score_points(Player , SC, PointsToScore) :-
     Points is SC + PointsToScore,
    retractall(sc(Player,_)),
    asserta(sc(Player,Points)).

% update_biggest_piece(+Player, +Piece, +BPR) updates the biggest piece removed by the player
update_biggest_piece(Player, Piece, BPR):-
        piece_size(Piece,Size),
        between(BPR,7, Size),
        retractall(bpr(Player,_)),
        asserta(bpr(Player,Size)).

% populate polulates the board with the initial pieces
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
              add_piece(16,3,d,90).

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

        R3 is 5 - L3,
        R4 is 4 - L4,
        R5 is 3 - L5,
        R6 is 2 - L6,
        R7 is 1 - L7,

        Length is R3 + R4 + R5 + R6 + R7.