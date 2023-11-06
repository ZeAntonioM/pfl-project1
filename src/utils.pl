:-ensure_loaded('pieces.pl').

%%%%%%%%%%%%% Write List %%%%%%%%%%%%%%%%%%%%%%%%
% write_list(+List) writes the elements of List
% When list is empty, it stops.
write_list_([]).  
% When List is not empty, it writes the head of the list
write_list_([Head | Tail]) :-
    write(Head), nl,  
    write_list_(Tail). 

%%%%%%%%%%%%% Print List %%%%%%%%%%%%%%%%%%%%%%%%
% print_string_(+List) converts the list of ascii codes to a string and writes it
% When list is empty, it stops.
print_string_([]):-!.
% When List is not empty, it converts the head of the list to a char and writes it
print_string_([Head | Rest]):-
        char_code(Char,Head),
        write(Char),
        print_string_(Rest).


%%%%%%%%%%%%% Convert Direction %%%%%%%%%%%%%%%%%%%%%%%%
% convert_direction(+Player, +Direction, -NewDirection) gives the direction after the conversion
convert_direction("W",l, r).
convert_direction("W",r, l).
convert_direction("W",u, d).
convert_direction("W",d, u).
convert_direction("B",Direction, Direction).


%%%%%%%%%%%%% Convert Position %%%%%%%%%%%%%%%%%%%%%%%%
% convert_position(+Player, +Position, -NewPosition) gives the position after the conversion
convert_position("B",Position,Position).
convert_position("W", Position, New_Position):- New_Position is 99- Position.


%%%%%%%%%%%%%  Next Position %%%%%%%%%%%%%%%%%%%%%%%%
% next_position(+Position, +Direction, -NextPosition) gives the position of the next cell
next_position(Position,l,Next_position):- Next_position is Position -1.
next_position(Position,r,Next_position):- Next_position is Position +1.
next_position(Position,u,Next_position):- Next_position is Position +10.
next_position(Position,d,Next_position):- Next_position is Position -10.


%%%%%%%%%%%%%  Generate Columns %%%%%%%%%%%%%%%%%%%%%%%%
% generate_columns(+N, -List) generates a list with the values of the column N
generate_columns(N,List):-
    generate_columns(N,List,9).

% generate_columns(+N, -List, +Acc) is an auxiliary predicate for generate_columns(+N, -List)
% If Acc is 0, it stops.
generate_columns(N, [N], 0).
% If Acc is not 0, it adds the value of the column to the list and decrements Acc
generate_columns(N, [N2 | List], Acc):-
        N2 is (Acc*10 + N),
        Acc2 is Acc -1,
        generate_columns(N, List, Acc2).

%%%%%%%%%%%%% Convert Score %%%%%%%%%%%%%%%%%%%%%%%%
% convert_SC(+Player, +SC, -New_SC) gives the score after the conversion
convert_SC("B",SC,SC).
convert_SC("W",SC, New_SC):-New_SC is 99- SC.


%%%%%%%%%%%%%%% List manipulation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%reverse_list(+List, -Rev) reverses the list
reverse_list([], []).
reverse_list([H|T], Reversed) :-
    reverse_list(T, ReversedTail),
    append(ReversedTail, [H], Reversed).

