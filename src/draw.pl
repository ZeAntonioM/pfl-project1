:-ensure_loaded('utils.pl').
:-ensure_loaded('pieces.pl').

write_val(N):-
        N<10,
        write(' 0'),
        write(N),
        write(' ').

write_val(N):-
        write(' '),
        write(N),
        write(' ').

draw_separation_line:- print_string_("|----|----|----|----|----|----|----|----|----|----|----|----|"), nl. 

write_piece(Player,Pos):-
        convert_position(Player,Pos,New_Pos),
        piece_position(Piece,Direction,New_Pos),!,
        convert_piece_to_string(Piece,Direction,Res),!,
        print_string_(Res).

write_piece(_,_):- print_string_("    ").

draw_line_content(Player,Line,Idx):-
        Idx<10,
        Pos is (9-Line)*10 + Idx,
        write_piece(Player,Pos),
        put_char('|'),
        Next_Idx is Idx +1,
        draw_line_content(Player,Line,Next_Idx).

draw_line_content(_,_,10).

draw_line(Player,Line):-
        Line<10,
        Val is Line*10,
        Val2 is 90 -Val,
        put_char('|'),
        write_val(Val2),
        put_char('|'),
        draw_line_content(Player,Line,0),
        write_val(Val),
        put_char('|'),
        nl.

draw_line(_,10).

draw_lines(_,10):-draw_separation_line.
draw_lines(Player,Line):-
        draw_separation_line,
        draw_line(Player,Line),
        Next_line is Line +1,
        draw_lines(Player,Next_line).

draw_board(Player ):-
        print_string_("  xx | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 | xx "),nl,
        draw_lines(Player,0),
        print_string_("  xx | 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | xx "),nl.
