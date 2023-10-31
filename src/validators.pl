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

valide_piece(Board, Player, Size, Piece) :-
    first_piece_of_size(Size, Player, First_Piece),
    Size2 is Size + 1,
    first_piece_of_size(Size2, Player, Last_Piece),
    !,
    (next_piece_not_on_board(Board, First_Piece, Last_Piece, Piece) ->
        true
    ; 
        write('No more Pieces from this size are left'), nl, fail
    ).
