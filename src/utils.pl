:-ensure_loaded('pieces.pl').

concatenate_lists([], List, List).
concatenate_lists([X|Xs], List2, [X|Result]) :-
    concatenate_lists(Xs, List2, Result).


print_string_([]):-!.
print_string_([Head | Rest]):-
        char_code(Char,Head),
        write(Char),
        print_string_(Rest).

convert_piece_to_string(Piece,h,Res):-
    get_Piece(Piece,String),
    concatenate_lists([8592], String, Result),
    concatenate_lists(Result, [8594],Res).

convert_piece_to_string(Piece,v,Res):-
    get_Piece(Piece,String),
    concatenate_lists([8593], String, Result),
    concatenate_lists(Result,[ 8595],Res).
    
get_Piece(Piece,Res):-
    piece_owener(Piece,Owener),
    piece_size(Piece,Size),
    Char_code is Size +48,
    concatenate_lists(Owener,[Char_code],Res).

convert_direction("W",l, r).
convert_direction("W",r, l).
convert_direction("W",u, d).
convert_direction("W",d, u).
convert_direction("B",Direction, Direction).
convert_position("B",Position,Position).
convert_position("W", Position, New_Position):- New_Position is 99- Position.
