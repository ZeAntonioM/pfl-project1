:-ensure_loaded('pieces.pl').
print_string_([]):-!.
print_string_([Head | Rest]):-
        char_code(Char,Head),
        write(Char),
        print_string_(Rest).

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
