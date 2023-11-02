:-ensure_loaded('pieces.pl').
print_string_([]):-!.
print_string_([Head | Rest]):-
        char_code(Char,Head),
        write(Char),
        print_string_(Rest).

convert_piece_to_string(Piece,h,Res):-
    get_Piece(Piece,String),
    append([8592], String, Result),
    append(Result, [8594],Res).

convert_piece_to_string(Piece,v,Res):-
    get_Piece(Piece,String),
    append([8593], String, Result),
    append(Result,[ 8595],Res).
    
get_Piece(Piece,Res):-
    piece_owner(Piece,Owner),
    piece_size(Piece,Size),
    Char_code is Size +48,
    append(Owner,[Char_code],Res).

convert_direction("W",l, r).
convert_direction("W",r, l).
convert_direction("W",u, d).
convert_direction("W",d, u).
convert_direction("B",Direction, Direction).

convert_position("B",Position,Position).
convert_position("W", Position, New_Position):- New_Position is 99- Position.

next_position(Position,l,Next_position):- Next_position is Position -1.
next_position(Position,r,Next_position):- Next_position is Position +1.
next_position(Position,u,Next_position):- Next_position is Position +10.
next_position(Position,d,Next_position):- Next_position is Position -10.
