:-ensure_loaded('utils.pl').

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


draw_board(Player):-
        print_string_("  xx | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 | xx "),nl,
        draw_lines(Player,0),
        print_string_("  xx | 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | xx "),nl.

player_to_move(Player):-
        nl,nl,
        write(' Player '), 
        print_string_(Player),
        write(' turn to play'),
        nl,nl.

draw_SC("B", SCB, SCW):-
        convert_SC("W", SCW, NEW_SCW),
        draw_SC(SCB, NEW_SCW).

draw_SC("W", SCB, SCW):-
        convert_SC("W", SCB, NEW_SCB),
        draw_SC(NEW_SCB, SCW).    

draw_SC(SCB, SCW):-
         nl,
         write('Black Player SC: '),
         write(SCB),
         write('                             '),
         write('White Player SC: '),
         write(SCW).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MENU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw_top_bottom_border :-
        write('|-----------------------------------------------------------------------------|'), nl.

draw_left_right_border :-
         write('|                                                                             |'), nl.

draw_isaac :-
        write('|   __________   ________        ____             ____            ________    |'), nl,
        write('|  |          | |        |      /    \\           /    \\         /         |   |'), nl,
        write('|   ---|  |---  |    ----      |  /\\  |         |  /\\  |       /          |   |'), nl,
        write('|      |  |     |   \\         /  /  \\  \\       /  /  \\  \\      |    ------    |'), nl,
        write('|      |  |      \\   --\\     |  /----\\  |     |  /----\\  |     |    |         |'), nl,
        write('|      |  |       \\--   \\   /           \\    /           \\     |    ______    |'), nl,
        write('|   ---|  |---  ___ /    | |  /-------\\  |  |  /-------\\  |    \\          |   |'), nl,
        write('|  | ________ ||________/ /__/         \\__\\/__/         \\__\\    \\---------    |'), nl,
        write('|                                                                             |'), nl.

draw_game_options :-
        draw_left_right_border,
        draw_left_right_border,
        write('|                              1. Player vs Player                            |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              2. Player vs IA                                |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              3. IA vs Player                                |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              4. IA vs IA                                    |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              5. Exit                                        |'), nl,
        draw_left_right_border,
        draw_left_right_border.


draw_isaac_menu :-
        draw_top_bottom_border,
        draw_isaac,
        draw_game_options,
        draw_top_bottom_border.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

