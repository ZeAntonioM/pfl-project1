encoding(utf8).

print_matrix([]).
print_matrix([Row|Rest]) :-
    print_row(Row),
    nl, % Newline to separate rows
    print_matrix(Rest).

print_string_([]):-!.
print_string_([Head | Rest]):-
        char_code(Char,Head),
        write(Char),
        print_string_(Rest).

% Predicate to print a row
print_row([]).
print_row([Cell|Rest]) :-
    write(Cell), % Print the current cell
    put_char('\t'), % Tab separator
    print_row(Rest).

invert([],Rev, Rev).
invert(List,Rev):- invert(List,[], Rev).
invert([Head | Rest], Acc, Rev):-
        invert(Rest, [Head | Acc], Rev).

invert_matrix([],Rev, Rev).
invert_matrix(List,Rev):- invert_matrix(List,[], Rev).
invert_matrix([Head | Rest], Acc, Rev):-
        invert(Head,Rev2),
        invert_matrix(Rest, [Rev2 | Acc], Rev).