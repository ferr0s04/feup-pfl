:- use_module(library(random)).
:- use_module('menu.pl').
:- use_module('board.pl').

% Entry point for the game
main :-
    menu.

% Start the game
start_game(Size, pvp) :-
    setup_board(Size),
    display_board(Size),
    game_loop(r, Size).

start_game(Size, pvc) :-
    setup_board(Size),
    display_board(Size),
    game_loop_pvc(r, Size).

start_game(Size, cvc) :-
    setup_board(Size),
    display_board(Size),
    game_loop_cvc(r, Size).

% Game loop for Player vs Computer
game_loop_pvc(Player, Size) :-
    (   Player = r ->
        write('Player r, make your move!'), nl,
        write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
        read([FromRow, FromCol, ToRow, ToCol]),
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size) ->
                (   next_player(Player, Opponent), check_win(Opponent) -> true
                ;   game_loop_pvc(b, Size)
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop_pvc(Player, Size)
        )
    ;   write('Computer is making a move...'), nl,
        computer_move(b, Size),
        (   check_win(Player) -> true
        ;   game_loop_pvc(r, Size)
        )
    ).

% Game loop for Computer vs Computer
game_loop_cvc(Player, Size) :-
    write('Computer '), write(Player), write(' is making a move...'), nl,
    computer_move(Player, Size),
    (   check_win(Player) ->  % Check if current player has lost
        write('Computer '), write(Player), write(' has no moves left. Game over!'), nl
    ;   next_player(Player, NextPlayer),
        game_loop_cvc(NextPlayer, Size)  % Switch to the next player
    ).

% Computer move logic using can_move/2
computer_move(Player, Size) :-
    % Try to find a stone for the computer to move
    (   board(FromRow, FromCol, Player),  % Find a stone controlled by the player
        can_move(FromRow, FromCol, ToRow, ToCol)  % Check if the stone can move
    ->  % If there is a valid move
        write('Computer moved from ['), write(FromRow), write(','), write(FromCol),
        write('] to ['), write(ToRow), write(','), write(ToCol), write('].'), nl,
        move(Player, FromRow, FromCol, ToRow, ToCol, Size),  % Apply the move on the board
        display_board(Size),  % Display the updated board
        check_win(Player)  % Check if the computer has won
    ;   write('No valid moves available for player '), write(Player), nl  % Handle no valid moves
    ).

% Game loop
game_loop(Player, Size) :-
    write('Player '), write(Player), write(', make your move!'), nl,
    write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
    read([FromRow, FromCol, ToRow, ToCol]),
    (   integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size) ->
                (   check_win(Player) -> true
                ;   next_player(Player, NextPlayer),
                    game_loop(NextPlayer, Size)
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop(Player, Size)
        )
    ;   write('Invalid input. Please enter integers.'), nl,
        game_loop(Player, Size)
    ).

% Switch to the next player
next_player(r, b).
next_player(b, r).

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

forall(between(1, Size, Row),
    forall(between(1, Size, Col),
        (   Condition1 -> Action1
        ;   Condition2 -> Action2
        ;   true
        )
    )
).

% Make a move for a player
move(Player, FromRow, FromCol, ToRow, ToCol, Size) :-
    valid_move(Player, FromRow, FromCol, ToRow, ToCol),
    retract(board(FromRow, FromCol, Player)),
    assertz(board(ToRow, ToCol, Player)),
    assertz(board(FromRow, FromCol, black)),
    handle_removals,
    display_board(Size).

% Check if the move is valid
valid_move(Player, FromRow, FromCol, ToRow, ToCol) :-
    board(FromRow, FromCol, Player),
    nonvar(Player), nonvar(FromRow), nonvar(FromCol), nonvar(ToRow), nonvar(ToCol),  % Guard against uninstantiated values
    queen_path_clear(FromRow, FromCol, ToRow, ToCol).

% Ensure path is clear for a chess queen move
queen_path_clear(FromRow, FromCol, ToRow, ToCol) :-
    DeltaRow is ToRow - FromRow,
    DeltaCol is ToCol - FromCol,
    (   DeltaRow =:= 0; DeltaCol =:= 0; abs(DeltaRow) =:= abs(DeltaCol) ) ->
        path_clear(FromRow, FromCol, DeltaRow, DeltaCol)
    ;   write('Invalid movement direction.'), nl, fail.

% Valid movement directions (horizontal, vertical, or diagonal)
valid_direction(DeltaRow, DeltaCol) :-
    between(-1, 1, DeltaRow),
    between(-1, 1, DeltaCol),
    \+ (DeltaRow =:= 0, DeltaCol =:= 0).  % Exclude "no movement"

% Check if the path is clear recursively
path_clear(Row, Col, 0, 0).
path_clear(Row, Col, DeltaRow, DeltaCol) :-
    nonvar(DeltaRow), nonvar(DeltaCol), % Ensure these are instantiated
    SignRow is sign(DeltaRow),
    SignCol is sign(DeltaCol),
    NextRow is Row + SignRow,
    NextCol is Col + SignCol,
    (   \+ board(NextRow, NextCol, _) -> % Check if the next position is clear
        NewDeltaRow is DeltaRow - SignRow,
        NewDeltaCol is DeltaCol - SignCol,
        path_clear(NextRow, NextCol, NewDeltaRow, NewDeltaCol)
    ;   write('Blocked at ['), write(NextRow), write(','), write(NextCol), write('].'), nl, fail
    ).

% Handle blocked stones (if applicable)
handle_removals :-
    write('Finding blocked stones...'), nl,
    findall([Row, Col, Player], blocked_stone(Row, Col, Player), StonesToRemove),
    remove_stones(StonesToRemove).

% Check if a stone is blocked
blocked_stone(Row, Col, Player) :-
    board(Row, Col, Player),
    nonvar(Row), nonvar(Col), nonvar(Player),  % Ensure inputs are instantiated
    \+ (valid_direction(DeltaRow, DeltaCol), % For all directions
        ToRow is Row + DeltaRow,
        ToCol is Col + DeltaCol,
        valid_move(Player, Row, Col, ToRow, ToCol)
    ),
    write('Stone is blocked.'), nl.

% Check if a stone can make any move
can_move(Row, Col) :-
    board(Row, Col, Player),
    write('Checking possible moves for stone at ['), write(Row), write(','), write(Col), write('] for player '), write(Player), nl,
    valid_direction(DeltaRow, DeltaCol),  % Generate potential move directions
    ToRow is Row + DeltaRow,
    ToCol is Col + DeltaCol,
    (   valid_move(Player, Row, Col, ToRow, ToCol) ->
        write('Can move to ['), write(ToRow), write(','), write(ToCol), write('].'), nl, !  % Cut after finding one valid move
    ;   fail
    ).

can_move(Row, Col, ToRow, ToCol) :-
    board(Row, Col, Player),
    write('Checking possible moves for stone at ['), write(Row), write(','), write(Col), write('] for player '), write(Player), nl,
    valid_direction(DeltaRow, DeltaCol),  % Generate potential move directions
    ToRow is Row + DeltaRow,
    ToCol is Col + DeltaCol,
    (   valid_move(Player, Row, Col, ToRow, ToCol) ->
        write('Can move to ['), write(ToRow), write(','), write(ToCol), write('].'), nl, !  % Cut after finding one valid move
    ;   fail
    ).

% Remove blocked stones
remove_stones([]).
remove_stones([[Row, Col, _] | Rest]) :-
    retract(board(Row, Col, _)),
    remove_stones(Rest).

% Remove all black stones (High Churn)
remove_all_black_stones :-
    retractall(board(_, _, B)).

% Remove contributing black stones (Medium Churn)
remove_contributing_black_stones(_) :-
    % Implementation of identifying contributing black stones goes here
    true.

% Winning condition
check_win(Player) :-
    \+ (board(Row, Col, Player), can_move(Row, Col)),  % No valid moves for player
    write('Player '), write(Player), write(' has no moves left. You lose!'), nl.
