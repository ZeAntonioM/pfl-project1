:-ensure_loaded('pieces.pl').
write_list_([]).  % Base case: an empty list, nothing to write.

write_list_([Head | Tail]) :-
    write(Head), nl,  % Write the current element and a newline.
    write_list_(Tail). % Recursively write the rest of the list.

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

generate_columns(N,List):-
    generate_columns(N,List,9).

generate_columns(N, [N], 0).
generate_columns(N, [N2 | List], Acc):-
        N2 is (Acc*10 + N),
        Acc2 is Acc -1,
        generate_columns(N, List, Acc2).

convert_SC("B",SC,SC).
convert_SC("W",SC, New_SC):-New_SC is 99- SC.

biggest_value(V1,V2,V1):- V1>=V2,!.
biggest_value(_V1,V2,V2):-!.

reverse_list([], []).
reverse_list([H|T], Reversed) :-
    reverse_list(T, ReversedTail),
    append(ReversedTail, [H], Reversed).

