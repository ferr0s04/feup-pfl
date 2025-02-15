% Initialize the game board
setup_board(Size, Board) :- 
    Size >= 6, 
    Size mod 2 =:= 0, 
    setup_perimeter(Size, Board).

% Setup the initial perimeter stones
setup_perimeter(Size, Board) :- 
    setup_perimeter_rows(Size, 1, [], Board).

setup_perimeter_rows(Size, Row, Acc, Board) :- 
    Row =< Size, 
    setup_perimeter_cols(Size, Row, 1, Acc, UpdatedAcc), 
    NextRow is Row + 1, 
    setup_perimeter_rows(Size, NextRow, UpdatedAcc, Board).
setup_perimeter_rows(_, _, Board, Board).

setup_perimeter_cols(Size, Row, Col, Acc, UpdatedAcc) :-
    Col =< Size,
    handle_column(Size, Row, Col, Acc, NewAcc),
    NextCol is Col + 1,
    setup_perimeter_cols(Size, Row, NextCol, NewAcc, UpdatedAcc).

setup_perimeter_cols(_, _, _, Acc, Acc).

handle_column(Size, Row, Col, Acc, NewAcc) :-
    Row = 1,
    Col mod 2 =:= 0,
    Col < Size,
    append(Acc, [(Row, Col, r)], NewAcc).

handle_column(Size, Row, Col, Acc, NewAcc) :-
    Row = Size,
    Col mod 2 =:= 1,
    Col > 1,
    append(Acc, [(Row, Col, r)], NewAcc).

handle_column(Size, Row, Col, Acc, NewAcc) :-
    Col = 1,
    Row mod 2 =:= 1,
    Row > 1,
    append(Acc, [(Row, Col, b)], NewAcc).

handle_column(Size, Row, Col, Acc, NewAcc) :-
    Col = Size,
    Row mod 2 =:= 0,
    Row < Size,
    append(Acc, [(Row, Col, b)], NewAcc).

handle_column(_, _, _, Acc, Acc).

% Display the board with grid
display_board(Size, Board) :- 
    nl,
    write('   |'), 
    display_column_labels(Size, 1),
    nl, 
    display_grid_line(Size), 
    nl, 
    display_rows(Size, 1, Board),
    nl.

% Helper to display column labels
display_column_labels(Size, Col) :-
    Col =< Size,
    display_column_label(Col),
    NextCol is Col + 1,
    display_column_labels(Size, NextCol).

display_column_labels(_, _).

display_column_label(Col) :-
    Col < 10,
    write(' '), write(Col), write(' |').

display_column_label(Col) :-
    Col >= 10,
    write(' '), write(Col), write('|').

% Helper to display the grid line
display_grid_line(Size) :- 
    Size > 0, 
    write('---+'),
    NextSize is Size - 1, 
    display_grid_line(NextSize).
display_grid_line(0) :- write('---+').

% Helper to display all rows
display_rows(Size, Row, Board) :-
    Row =< Size,
    display_row_label(Row),
    display_columns(Size, Row, 1, Board),
    nl,
    display_grid_line(Size),
    nl,
    NextRow is Row + 1,
    display_rows(Size, NextRow, Board).

display_rows(_, _, _).

display_row_label(Row) :-
    Row < 10,
    write(' '), write(Row), write(' |').

display_row_label(Row) :-
    Row >= 10,
    write(Row), write(' |').

% Helper to display a single row's columns
display_columns(Size, Row, Col, Board) :- 
    Col =< Size, 
    (   member((Row, Col, Stone), Board) -> 
        (   Stone = black -> write(' X |') 
        ;   write(' '), write(Stone), write(' |')
        )
    ;   write('   |')
    ),
    NextCol is Col + 1, 
    display_columns(Size, Row, NextCol, Board).
display_columns(_, _, _, _).
