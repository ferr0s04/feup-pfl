:- use_module(library(random)).
:- use_module('menu.pl').
:- use_module('board.pl').

% Entry point for the game
main :-
    menu.

% Start the game
start_game(Size, pvp) :-
    setup_game(Size, Board),
    display_game(Size, Board),
    game_loop(r, Size, Board).

start_game(Size, pvc) :-
    setup_game(Size, Board),
    display_game(Size, Board),
    game_loop_pvc(r, Board).

start_game(Size, cvc) :-
    setup_game(Size, Board),
    display_game(Size, Board),
    game_loop_cvc(r, Board).


% Game loop
game_loop(Player, Size, Board) :-
    write('Player '), write(Player), write(', make your move!'), nl,
    write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
    read([FromRow, FromCol, ToRow, ToCol]),
    (   integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard) ->
                (   check_win(Player, UpdatedBoard) -> true
                ;   next_player(Player, NextPlayer),
                    game_loop(NextPlayer, Size, UpdatedBoard)
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop(Player, Size, Board)
        )
    ;   write('Invalid input. Please enter integers.'), nl,
        game_loop(Player, Size, Board)
    ).


% Game loop for Player vs Computer
game_loop_pvc(Player, Size, Board) :-
    (   Player = r ->
        write('Player r, make your move!'), nl,
        write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
        read([FromRow, FromCol, ToRow, ToCol]),
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard) ->
                (   next_player(Player, Opponent), check_win(Opponent, UpdatedBoard) -> true
                ;   game_loop_pvc(b, Size, UpdatedBoard)
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop_pvc(Player, Size, Board)
        )
    ;   write('Computer is making a move...'), nl,
        computer_move(b, Size, Board, UpdatedBoard),
        (   check_win(Player, UpdatedBoard) -> true
        ;   game_loop_pvc(r, Size, UpdatedBoard)
        )
    ).

% Game loop for Computer vs Computer
game_loop_cvc(Player, Size, Board) :-
    write('Computer '), write(Player), write(' is making a move...'), nl,
    computer_move(Player, Size, Board, UpdatedBoard),
    (   check_win(Player, UpdatedBoard) ->  % Check if current player has lost
        write('Computer '), write(Player), write(' has no moves left. Game over!'), nl
    ;   next_player(Player, NextPlayer),
        game_loop_cvc(NextPlayer, Size, UpdatedBoard)  % Switch to the next player
    ).

% Computer move logic using can_move/2
computer_move(Player, Size, Board, UpdatedBoard) :-
    % Try to find a stone for the computer to move
    (   member((FromRow, FromCol, Player), Board),  % Find a stone controlled by the player
        can_move(FromRow, FromCol, ToRow, ToCol, Board)  % Check if the stone can move
    ->  % If there is a valid move
        write('Computer moved from ['), write(FromRow), write(','), write(FromCol),
        write('] to ['), write(ToRow), write(','), write(ToCol), write('].'), nl,
        move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard),  % Apply the move on the board
        display_board(Size, UpdatedBoard)  % Display the updated board
    ;   write('No valid moves available for player '), write(Player), nl,
        UpdatedBoard = Board  % No moves available, return the board unchanged
    ).

% Switch to the next player
next_player(r, b).
next_player(b, r).

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

% Make a move for a player
move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard) :-
    valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board),
    update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, IntermediateBoard),
    handle_removals(IntermediateBoard, UpdatedBoard),
    display_game(Size, UpdatedBoard).

% Update the board after a move
update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, UpdatedBoard) :-
    replace(Board, (FromRow, FromCol, Player), (FromRow, FromCol, black), TempBoard),
    replace(TempBoard, (ToRow, ToCol, black), (ToRow, ToCol, Player), UpdatedBoard).

% Replace an element in the board
replace([Old | Rest], Old, New, [New | Rest]).
replace([Other | Rest], Old, New, [Other | UpdatedRest]) :-
    Other \= Old,
    replace(Rest, Old, New, UpdatedRest).


% Check if the move is valid
valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board) :-
    member((FromRow, FromCol, Player), Board),
    queen_path_clear(FromRow, FromCol, ToRow, ToCol, Board).


% Ensure path is clear for a chess queen move
queen_path_clear(FromRow, FromCol, ToRow, ToCol, Board) :-
    DeltaRow is ToRow - FromRow,
    DeltaCol is ToCol - FromCol,
    (   DeltaRow =:= 0; DeltaCol =:= 0; abs(DeltaRow) =:= abs(DeltaCol) ) ->
        path_clear(FromRow, FromCol, DeltaRow, DeltaCol, Board)
    ;   write('Invalid movement direction.'), nl, fail.

% Valid movement directions (horizontal, vertical, or diagonal)
valid_direction(DeltaRow, DeltaCol) :-
    between(-1, 1, DeltaRow),
    between(-1, 1, DeltaCol),
    \+ (DeltaRow =:= 0, DeltaCol =:= 0).  % Exclude "no movement"

% Check if the path is clear recursively
path_clear(Row, Col, 0, 0, _).
path_clear(Row, Col, DeltaRow, DeltaCol, Board) :-
    SignRow is sign(DeltaRow),
    SignCol is sign(DeltaCol),
    NextRow is Row + SignRow,
    NextCol is Col + SignCol,
    (   \+ member((NextRow, NextCol, _), Board) ->
        NewDeltaRow is DeltaRow - SignRow,
        NewDeltaCol is DeltaCol - SignCol,
        path_clear(NextRow, NextCol, NewDeltaRow, NewDeltaCol, Board)
    ;   write('Blocked at ['), write(NextRow), write(','), write(NextCol), write('].'), nl, fail
    ).

% Handle blocked stones (if applicable)
handle_removals(Board, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), StonesToRemove),
    remove_stones(StonesToRemove, Board, UpdatedBoard).

% Check if a stone is blocked
blocked_stone(Row, Col, Player, Board) :-
    member((Row, Col, Player), Board),
    \+ (valid_direction(DeltaRow, DeltaCol),
        ToRow is Row + DeltaRow,
        ToCol is Col + DeltaCol,
        valid_move(Player, Row, Col, ToRow, ToCol, Board)
    ).
    write('Stone is blocked.'), nl.

% Remove blocked stones from the board
remove_stones([], Board, Board).
remove_stones([(Row, Col, _) | Rest], Board, UpdatedBoard) :-
    delete(Board, (Row, Col, _), TempBoard),
    remove_stones(Rest, TempBoard, UpdatedBoard).

% Check if a stone can move
can_move(Row, Col, ToRow, ToCol, Board) :-
    member((Row, Col, Player), Board),
    valid_direction(DeltaRow, DeltaCol),
    ToRow is Row + DeltaRow,
    ToCol is Col + DeltaCol,
    valid_move(Player, Row, Col, ToRow, ToCol, Board).

% Utility to delete an element from the board
delete([Elem | Rest], Elem, Rest).
delete([Other | Rest], Elem, [Other | UpdatedRest]) :-
    Other \= Elem,
    delete(Rest, Elem, UpdatedRest).


% Remove all black stones (High Churn)
remove_all_black_stones :-
    retractall(board(_, _, B)).

% Remove contributing black stones (Medium Churn)
remove_contributing_black_stones(_) :-
    % Implementation of identifying contributing black stones goes here
    true.

% Check win condition
check_win(Player, Board) :-
    \+ (member((Row, Col, Player), Board), can_move(Row, Col, _, _, Board)),  % No valid moves for player
    write('Player '), write(Player), write(' has no moves left. You lose!'), nl.
