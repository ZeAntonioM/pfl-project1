:-consult('pieces.pl').
concatenate_lists([], List, List).
concatenate_lists([X|Xs], List2, [X|Result]) :-
    concatenate_lists(Xs, List2, Result).



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


convert(Piece-h, String):-
    get_Piece(Piece,String2),
    concatenate_lists([8592], String2, Result),
    concatenate_lists(Result, [8594],String).

convert(Piece-v, String):-
    get_Piece(Piece,String2),
    concatenate_lists([8593], String2, Result),
    concatenate_lists(Result,[ 8595],String).
convert(_, "    ").



convert_list([], []).
convert_list([Piece|Xs], [Y|Ys]) :-
    convert(Piece, Y),
    convert_list(Xs, Ys).

list_to_matrix(List, Matrix) :-
    length(List, 100), % Ensure the input list has the correct size
    convert_list(List,ConvertedList),
    list_to_matrix(ConvertedList, 10, 10, Matrix).

list_to_matrix([], _, 0, []).
list_to_matrix(List, NumCols, NumRows, [Row|Matrix]) :-
    append(Row, Rest, List),
    length(Row, NumCols),
    NewNumRows is NumRows - 1,
    list_to_matrix(Rest, NumCols, NewNumRows, Matrix).

get_Piece(Piece,Res):-
    piece_owener(Piece,Owener),
    piece_size(Piece,Size),
    Char_code is Size +48,
    concatenate_lists(Owener,[Char_code],Res).
    
