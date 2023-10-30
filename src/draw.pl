:-consult('utils.pl').

write_val(N):-
        N<10,
        write(' '),
        write(0),
        write(N),
        write(' ').

write_val(N):-
        N>=10,
        write(' '),
        write(N),
        write(' ').

draw_number_line(0):-
        print_string_("  xx | 00 "),
        
        draw_number_line(1),
        put_char('|'),
        print_string_(" xx "),
        nl.
        
       
draw_number_line(N):-
        N <10,
        put_char('|'),
        write_val(N),
        N2 is N + 1,
        draw_number_line(N2).

draw_number_line(10).


draw_inverse_number_line(10):-
        print_string_("  xx "),
        draw_inverse_number_line(9),
        put_char('|'),
        print_string_(" xx "),
        nl.
        
       
draw_inverse_number_line(N):-
        N >0,
        put_char('|'),
        write_val(N),
        N2 is N - 1,
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
    %put_char(' '),
    print_string_(Head),
    print_string_("|").

draw_line_content([Head|Rest]):-
    %put_char(''),
    print_string_(Head),
    print_string_("|"),
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
        draw_inverse_number_line(10),
        list_to_matrix(Pieces,Matrix),
        draw_lines(Matrix,0),
        draw_number_line(0).

draw_board(Pieces,0 ):-
        draw_inverse_number_line(10),
        list_to_matrix(Pieces,Matrix),
        invert_matrix(Matrix, Rev),
        draw_lines(Rev,0),
        draw_number_line(0).