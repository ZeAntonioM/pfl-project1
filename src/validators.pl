:-ensure_loaded('utils.pl').
:- use_module(library(between)).


validate_size(Size):-
        between(3,7,Size).

validate_direction(Direction):-
        Direction = d;
        Direction = u;
        Direction = l;
        Direction = r.

validate_position(Position):-
        between(0,99,Position).

valid_piece( Player, Size, Piece) :-
    first_piece_of_size(Size, Player, First_Piece),
    Size2 is Size + 1,
    first_piece_of_size(Size2, Player, Last_Piece),
    !,
    next_piece_not_on_board( First_Piece, Last_Piece, Piece).


valid_position(1,Position,_):-
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position).
        
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

valid_position(Size,Position,Direction):-
         (Direction = u;
        Direction = d),
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position),
        next_position(Position,Direction,Next_position),
        Size2 is Size -1,
        valid_position(Size2,Next_position,Direction).

validate_menu_choice(Choice):-
        Choice >= 1,
        Choice =< 10.

validate_piece_to_remove(Piece):-
        piece_position(Piece,_Direction,_Position).


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

