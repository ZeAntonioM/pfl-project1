:-ensure_loaded('validators.pl').

can_place_piece(Player,P,Size,Direction):-
        can_place_piece(Player, P, Size,Direction, 3).

can_place_piece(Player,P,Acc,Direction, Acc):-
        valid_position(Acc,P,Direction),
        valid_piece(Player, Acc,_Piece).

can_place_piece(Player,P,Size,Direction, Acc):-
        validate_size(Acc),
        Acc2 is Acc +1,
        can_place_piece(Player,P,Size,Direction, Acc2).


populate(Pos1, Pos2):-
        assertz((piece_position(_,_,_):-fail)),
        Pos1<Pos2,
        asserta(piece_position(1,h,Pos1)),
        Pos3 is Pos1 +1,
        populate(Pos3,Pos2).

populate(Pos1,Pos1).