:-consult('pieces.pl').


validate_size(Size):-
        Size>2,
        Size<8.

validate_direction(Direction):-
        Direction = d;
        Direction = u;
        Direction = l;
        Direction = r.

validate_position(Position):-
        Position >=0,
        Position =<99.

valide_piece( Player, Size, Piece) :-
    first_piece_of_size(Size, Player, First_Piece),
    Size2 is Size + 1,
    first_piece_of_size(Size2, Player, Last_Piece),
    !,
    next_piece_not_on_board( First_Piece, Last_Piece, Piece).

valide_position(1,Position,_):-
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position).
        
valide_position(Size,Position,Direction):-
        (Direction = l;
        Direction = r),
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position),
        Line is truncate(Position / 10),
        Size2 is Size -1,
        next_position(Position,Direction,Next_position),
        Next_line is truncate((Next_position) / 10),
        Next_line = Line,
        valide_position(Size2,Next_position,Direction).

valide_position(Size,Position,Direction):-
         (Direction = u;
        Direction = d),
        validate_position(Position),
        \+ piece_position(_Piece,_Direction,Position),
        next_position(Position,Direction,Next_position),
        Size2 is Size -1,
        valide_position(Size2,Next_position,Direction).

next_position(Position,l,Next_position):- Next_position is Position -1.
next_position(Position,r,Next_position):- Next_position is Position +1.
next_position(Position,u,Next_position):- Next_position is Position +10.
next_position(Position,d,Next_position):- Next_position is Position -10.
        



