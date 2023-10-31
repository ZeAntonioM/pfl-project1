:-consult('validators.pl').


ask_for_piece_to_add(Player, Board):-
        repeat,
        ask_for_piece_to_add_message(Size, _Direction, _Position),
        valide_piece(Board, Player, Size,_Piece).



ask_for_piece_to_add(Player, Board):-
             ask_for_piece_to_add(Player,Board).


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


        