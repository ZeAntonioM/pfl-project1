print_matrix([]).
print_matrix([Row|Rest]) :-
    print_row(Row),
    nl, % Newline to separate rows
    print_matrix(Rest).
write_val(0):-print_string_(" 00 ").
write_val(N):-
        write(' '),
        write(N),
        write(' ').
% Predicate to print a row
print_row([]).
print_row([Cell|Rest]) :-
    write(Cell), % Print the current cell
    put_char('\t'), % Tab separator
    print_row(Rest).
        
print_string_([]):-!.
print_string_([Head | Rest]):-
        char_code(Char,Head),
        write(Char),
        print_string_(Rest).




invert([],Rev, Rev).
invert(List,Rev):- invert(List,[], Rev).
invert([Head | Rest], Acc, Rev):-
        invert(Rest, [Head | Acc], Rev).

invert_matrix([],Rev, Rev).
invert_matrix(List,Rev):- invert_matrix(List,[], Rev).
invert_matrix([Head | Rest], Acc, Rev):-
        invert(Head,Rev2),
        invert_matrix(Rest, [Rev2 | Acc], Rev).

draw_number_line(0):-
        print_string_("  xx "),
        put_char('|'),
        write_val(0),
        draw_number_line(10).

draw_number_line(100):-
        put_char('|'),
        print_string_(" xx "),
        nl.
       
draw_number_line(N):-
        N < 100,
        put_char('|'),
        write_val(N),
        N2 is N + 10,
        draw_number_line(N2).


draw_inverse_number_line(100):-
        print_string_("  xx "),
        draw_inverse_number_line(90),
        put_char('|'),
        print_string_(" xx "),
        
nl.
        
       
draw_inverse_number_line(N):-
        N >0,
        put_char('|'),
        write_val(N),
        N2 is N - 10,
        draw_inverse_number_line(N2).

draw_inverse_number_line(0):-
        write('|'),
        write_val(0).
draw_separation_line(N):-
    N=<0,
    put_char('|'),
    nl.
draw_separation_line(N):-
        N >0,
        print_string_("|----"),
        N2 is N-3,
        draw_separation_line(N2).

draw_line_content([Head]):-
    put_char(' '),
    print_string_(Head),
    print_string_(" |").

draw_line_content([Head|Rest]):-
    put_char(' '),
    print_string_(Head),
    print_string_(" |"),
    draw_line_content(Rest).

draw_line(100).                   
draw_line(List,Acc):-
        Acc<100,
        Val is 90 -Acc,
        put_char('|'),
        write_val(Val),
        put_char('|'),
        draw_line_content(List),
        write_val(Acc),
        put_char('|'),
        nl.

draw_lines([],_):-draw_separation_line(35).
draw_lines([Row|Rest],Acc):-
        draw_separation_line(35),
        draw_line(Row,Acc),
        Acc2 is Acc +10,
        draw_lines(Rest, Acc2).

draw_board(Pieces,1 ):-
        draw_inverse_number_line(100),
        draw_lines(Pieces,0),
        draw_number_line(0).

draw_board(Pieces,0 ):-
        draw_inverse_number_line(100),
        invert_matrix(Pieces, Rev),
        draw_lines(Rev,0),
        draw_number_line(0).

