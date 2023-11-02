:-ensure_loaded('validators.pl').
:-ensure_loaded('draw.pl').
ask_for_piece_to_add(Player):-
        assertz((piece_position(_,_,_):-fail)),
        repeat,
        ask_for_piece_to_add_message(Size, Direction, Position),
        convert_direction(Player,Direction, New_Direction),
        convert_position(Player, Position, New_Position),
        (valid_piece( Player, Size,Piece)->
                true;
                write('No more Pieces from this size are left'), nl, fail
         ),
        (valid_position(Size,New_Position,New_Direction)->
         true;
         write('This Piece can not be placed like that'),nl,fail
         ),
        add_piece(Piece,Size,New_Direction, New_Position),
        draw_board(Player).


ask_for_piece_to_add_message(Size, Direction, Position):-
        ask_for_piece_size(Size),!,
        ask_for_piece_direction(Direction),!,
        ask_for_piece_position(Position),!.

ask_for_piece_size(Size):-
        write('Select the piece size, It should be between 3 and 7:'),
        read(Size),
        validate_size(Size).

ask_for_piece_size(Size):-
        write('Invalid Size'),nl,
        ask_for_piece_size(Size).

ask_for_piece_direction(Direction):-
        write('Select the piece direction, It shoud be u(up), d(down), r(right), l (left) : '),
        read(Direction),
        validate_direction(Direction).

ask_for_piece_direction(Direction):-
        write('Invalid Direction'),nl,
        ask_for_piece_direction(Direction).

ask_for_piece_position(Position):-
        write('Select the piece position, It shoud be between 0 and 99:'),
        read(Position),
        validate_position(Position).

ask_for_piece_position(Position):-
        write('Invalid Position'),nl,
        ask_for_piece_position(Position).


        


isaac_menu:-
    draw_isaac_menu,
    % Get the user's choice
    write('Choose an option(remember to always put a . at the end of the number): ')
    read(Choice),
    % Handle the user's choice
    (Choice = 1 -> player_vs_player;
     Choice = 2 -> player_vs_ia;
     Choice = 3 -> ia_vs_ia;
     Choice = 4 -> true;
     isaac_menu).

