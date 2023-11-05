:-ensure_loaded('validators.pl').
:-ensure_loaded('logic.pl').
:-ensure_loaded('draw.pl').
:-ensure_loaded('logic.pl').

ask_for_piece_to_add(Player, Piece, New_Direction , New_Position):-

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
         ).


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
    
ask_for_menu_option(Choice):-
    write('Choose an option(1-10): '),
    write('A '),
    read(Choice), nl,
    write('B '), write(Choice), nl,
    validate_menu_choice(Choice).
    
ask_for_menu_option(Choice):-
        write('Invalid choice! '),
        write(Choice), nl,
        ask_for_menu_option(Choice).

% Gets the points to score
get_points_to_score(Points, PointsToScore) :-
    write('You have removed a piece from the board and can score from 0 to '), write(Points), write(' points.'), nl,
    write('Choose the number of points you want to score: '), nl,
    read(PointsToScore),
    PointsToScore >= 0,
    PointsToScore =< Points.

get_points_to_score(Points, PointsToScore) :-
    write('Invalid number of points. Try again.'), nl,
    get_points_to_score(Points, PointsToScore).


ask_for_piece_to_remove(Player, Piece, Direction, New_Position):-
        repeat,
        ask_for_piece_to_remove_message( Position),
        convert_position(Player, Position, New_Position),
        (piece_position(Piece, _, New_Position)->
                true;
                write('Position is empty'),fail
         ),
        (can_remove_piece(Player, Piece)->
                piece_position(Piece, Direction, _);
                write('You cannot remove this piece.'), nl, fail
        ).
    


ask_for_piece_to_remove_message( Position):-
        write('Select the position of the piece to remove. It shoud be between 0 and 99:'),
        read(Position),
        validate_position( Position).
        

ask_for_piece_to_remove_message( Position):-
        nl, write('Invalid Piece'),nl,
        ask_for_piece_to_remove_message(Position).
