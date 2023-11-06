:-ensure_loaded('utils.pl').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DISPLAY BOARD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% convert_piece_to_string(Piece,Direction,Res) receives a piece and puts it with a new format in Res.
% If piece is placed horizontally, the direction is h.
% Gets the string for the Piece and ads an arrow to the left at the beggining and an arrow to the right at the end.
convert_piece_to_string(Piece,h,Res):-
    get_Piece(Piece,String),
    append([8592], String, Result),
    append(Result, [8594],Res).

% If piece is placed vertically, the direction is v.
% Gets the string for the Piece and ads an arrow to the top at the beggining and an arrow to the bottom at the end.
convert_piece_to_string(Piece,v,Res):-
    get_Piece(Piece,String),
    append([8593], String, Result),
    append(Result,[ 8595],Res).

% get_piece(Piece,Res) gets the piece and converts it to a string.   
% Checks Who is the owner of the piece, checks the size of the piece and converts it to a string, that is placed in Res.
get_Piece(Piece,Res):-
    piece_owner(Piece,Owner),
    piece_size(Piece,Size),
    Char_code is Size +48,
    append(Owner,[Char_code],Res).

% write_val(N) writes the value N in the board.
% if the value is shorter than 10, it adds a 0 to the left and writes the value with a space at the beggining and at the end.
write_val(N):-
        N<10,
        write(' 0'),
        write(N),
        write(' ').

% if the value is bigger than 10, it writes the value with a space at the beggining and at the end.
write_val(N):-
        write(' '),
        write(N),
        write(' ').

% draw_separation_line writes the separation line of the the board.
draw_separation_line:- print_string_("|----|----|----|----|----|----|----|----|----|----|----|----|"), nl. 

% write_piece(Player,Pos) receives a player and a position and writes the piece in the board.
% It coonverts the position to the final position, verifies if there is a piece in that position and if there is, it converts it to a string and writes it in the board.
write_piece(Player,Pos):-
        convert_position(Player,Pos,New_Pos),
        piece_position(Piece,Direction,New_Pos),!,
        convert_piece_to_string(Piece,Direction,Res),!,
        print_string_(Res).

% If there is no piece in that position, it writes a free space in the board.
write_piece(_,_):- print_string_("    ").

% draw_line_content(Player,Line,Idx) receives a player, a line and an index and writes the content of the line.
% Checks if the index is smaller than 10. If it is, it writes in the board the piece or the free space and calls the next index.
draw_line_content(Player,Line,Idx):-
        Idx<10,
        Pos is (9-Line)*10 + Idx,
        write_piece(Player,Pos),
        put_char('|'),
        Next_Idx is Idx +1,
        draw_line_content(Player,Line,Next_Idx).

% If the index is 10, than it reached the end of the line.
draw_line_content(_,_,10).

% draw_line(Player,Line) receives a player and a line and writes the line in the board.
% Check if the Line is smaller than 10. If it is, it writes the number of the line, and calls the draw_line_content to write the content of the line.
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

% If the line is 10, than it reached the end of the board.
draw_line(_,10).

% draw_lines(Player,Line) receives a player and a line and writes the lines in the board.
% If the Line is 10, than it reached the end of the board, and just writes the last line.
draw_lines(_,10):-draw_separation_line.

% If the Line is smaller than 10, it writes the line and calls the next line.
draw_lines(Player,Line):-
        draw_separation_line,
        draw_line(Player,Line),
        Next_line is Line +1,
        draw_lines(Player,Next_line).


% draw_board(Player) receives a player and draws the board.
draw_board(Player):-
        nl,
        print_string_("  xx | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 | xx "),nl,
        draw_lines(Player,0),
        print_string_("  xx | 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | xx "),nl.

% player_to_move(Player) receives a player and writes that it is his turn to play.
player_to_move(Player):-
        nl,nl,
        write(' Player '), 
        print_string_(Player),
        write(' turn to play'),
        nl,nl.

% draw_SC(Player, SCB, SCW) receives a player and the SC of both players and writes it in the board.
% if the player is black, it writes the SC of the black player and converts the SC of the white player to the new format.
draw_SC("B", SCB, SCW):-
        convert_SC("W", SCW, NEW_SCW),
        draw_SC(SCB, NEW_SCW).

% if the player is white, it writes the SC of the white player and converts the SC of the black player to the new format.
draw_SC("W", SCB, SCW):-
        convert_SC("W", SCB, NEW_SCB),
        draw_SC(NEW_SCB, SCW).    

% draw_SC(SCB, SCW) receives the SC of both players and writes it in the board.
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

% draw_top_bottom_border draws a line that is used at the top and at the bottom of the menu.
draw_top_bottom_border :-
        write('|-----------------------------------------------------------------------------|'), nl.

% draw_left_right_border draws a line that limits the left and right side of the menu.
draw_left_right_border :-
         write('|                                                                             |'), nl.

% draw_isaac draws the name of the game.
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

% draw_game_options draws the options of the menu.
draw_game_options :-
        draw_left_right_border,
        draw_left_right_border,
        write('|                              1. Player vs Player                            |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              2. Player vs Easy IA                           |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              3. Easy IA vs Player                           |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              4. Player vs Hard IA                           |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              5. Hard IA vs Player                           |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              6. Easy IA vs Hard IA                          |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              7. Hard IA vs Easy IA                          |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              8. Easy IA vs Easy IA                          |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              9. Hard IA vs Hard IA                          |'), nl,
        draw_left_right_border,
        draw_left_right_border,
        write('|                              10. Exit                                       |'), nl,
        draw_left_right_border,
        draw_left_right_border.

% draw_isaac_menu draws the menu.
draw_isaac_menu :-
        draw_top_bottom_border,
        draw_isaac,
        draw_game_options,
        draw_top_bottom_border.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

