:-ensure_loaded('utils.pl').
:- use_module(library(between)).

% validate_size(+Size) checks if the size is between 3 and 7
validate_size(Size):-
        between(3,7,Size).

% validate_direction(+Direction) checks if the direction is one of the four possible directions
validate_direction(Direction):-
        Direction = d;
        Direction = u;
        Direction = l;
        Direction = r.

% validate_position(+Position) checks if the position is between 0 and 99
validate_position(Position):-
        between(0,99,Position).

% valid_piece(+Player, +Size, -Piece) returns a valid piece for the player and size if exists
valid_piece( Player, Size, Piece) :-
    first_piece_of_size(Size, Player, First_Piece),
    Size2 is Size + 1,
    first_piece_of_size(Size2, Player, Last_Piece),
    !,
    next_piece_not_on_board( First_Piece, Last_Piece, Piece).

% valid_position(+Size, +Position, +Direction) checks if the position is valid for the size and direction
% If the size is 1, it checks if the position is valid and if there is no piece in that position
valid_position(1,Position,_):-
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position).
        
% If the size is bigger than 1 and the direction is left or right, it checks if the position is valid and if there is no piece in that position, and
% if the next position is inside the board and in the same line.
valid_position(Size,Position,Direction):-
        (Direction = l;
        Direction = r),
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position),
        Line is truncate(Position / 10),
        Size2 is Size -1,
        next_position(Position,Direction,Next_position),
        Next_line is truncate((Next_position) / 10),
        Next_line = Line,
        valid_position(Size2,Next_position,Direction).

% If the size is bigger than 1 and the direction is up or down, it checks if the position is valid and if there is no piece in that position, and
% if the next position is inside the board and in the same column.
valid_position(Size,Position,Direction):-
         (Direction = u;
        Direction = d),
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position),
        next_position(Position,Direction,Next_position),
        Size2 is Size -1,
        valid_position(Size2,Next_position,Direction).

% validate_menu_choice(+Choice) checks if the choice is between 1 and 10
validate_menu_choice(Choice):-
        Choice >= 1,
        Choice =< 5.

% validate_piece_to_remove(+Piece) checks if the piece is on the board
validate_piece_to_remove(Piece):-
        piece_position(Piece,_Direction,_Position).

% can_move_piece(+Player, +Piece, +Direction) checks if the player is the owner of the piece, gets the piece positions, the Score Counters positions
% and the Biggest Piece removed, and checks if the piece size is bigger than the Biggest Piece removed and has no Score Counters in the top of the piece
can_remove_piece(Player,Piece):-
    piece_owner(Piece,Player),
    findall(Position, piece_position(Piece, _, Position),  Positions),
    sc("W",SCW),
    sc("B",SCB),
    bpr(Player,BPR),
    piece_size(Piece, SizeP),!,
    New_SCW is 99 - SCW,
    SizeP >= BPR,!,
    \+ member(SCB, Positions),!,
    \+ member(New_SCW, Positions),!.

