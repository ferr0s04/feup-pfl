:- use_module(library(random)).
:- consult(menu).
:- consult(board).

% Entry point for the game
play :-
    menu.

% Define the initial game state
initial_state(GameConfig, GameState) :-
    GameConfig = game_config(variant(Variant), game_type(GameType), difficulty(Difficulty), size(Size), current_player(CurrentPlayer)),
    GameState = game_state(board(Board), current_player(CurrentPlayer), size(Size)),
    display_game(GameState, GameType, Variant, Difficulty, Size).

% Adjusted game start logic to pass variants and difficulties
display_game(GameState, GameType, Variant, Difficulty, Size) :-
    GameState = game_state(board(Board), current_player(CurrentPlayer), size(Size)),
    display_board(Size, Board),
    (   GameType == pvp ->
        game_loop_pvp(CurrentPlayer, Size, Board, Variant)
    ;   GameType == pvc ->
        game_loop_pvc(CurrentPlayer, Size, Board, Variant, Difficulty)
    ;   GameType == cvc ->
        game_loop_cvc(CurrentPlayer, Size, Board, Variant, Difficulty)
    ).

% Game loop for Player vs Player
game_loop_pvp(Player, Size, Board, Variant) :-
    write('Player '), write(Player), write(', make your move!'), nl,
    write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
    read([FromRow, FromCol, ToRow, ToCol]),
    (   integer(FromRow), integer(FromCol), integer(ToRow), integer(ToCol) ->
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, IntermediateBoard) ->
                (   handle_removals(IntermediateBoard, Variant, UpdatedBoard),
                    (   game_over(Player, UpdatedBoard) -> true
                    ;   next_player(Player, NextPlayer),
                        game_loop_pvp(NextPlayer, Size, UpdatedBoard, Variant)
                    )
                )
        ;   write('Invalid move. Try again.'), nl,
            game_loop_pvp(Player, Size, Board, Variant)
        )
    ;   write('Invalid input. Please enter integers.'), nl,
        game_loop_pvp(Player, Size, Board, Variant)
    ).

% Game loop for Player vs Computer
game_loop_pvc(Player, Size, Board, Variant, Difficulty) :-
    (   Player = r ->
        write('Player r, make your move!'), nl,
        write('Enter [FromRow, FromCol, ToRow, ToCol]: '),
        read([FromRow, FromCol, ToRow, ToCol]),
        (   move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, IntermediateBoard) ->
            handle_removals(IntermediateBoard, Variant, UpdatedBoard),
            (   next_player(Player, Opponent),
                game_over(Opponent, UpdatedBoard) -> true
            ;   game_loop_pvc(b, Size, UpdatedBoard, Variant, Difficulty)
            )
        ;   write('Invalid move. Try again.'), nl,
            game_loop_pvc(Player, Size, Board, Variant, Difficulty)
        )
    ;   write('Computer is making a move...'), nl,
        computer_move(b, Size, Board, IntermediateBoard, Difficulty),
        handle_removals(IntermediateBoard, Variant, UpdatedBoard),
        (   game_over(Player, UpdatedBoard) -> true
        ;   game_loop_pvc(r, Size, UpdatedBoard, Variant, Difficulty)
        )
    ).

% Game loop for Computer vs Computer 
game_loop_cvc(Player, Size, Board, Variant, Difficulty) :-
    write('Computer '), write(Player), write(' is making a move...'), nl,
        computer_move(Player, Size, Board, IntermediateBoard, Difficulty),  
        handle_removals(IntermediateBoard, Variant, UpdatedBoard),
    (   game_over(Player, UpdatedBoard) -> true
    ;   next_player(Player, NextPlayer),
        game_loop_cvc(NextPlayer, Size, UpdatedBoard, Variant, Difficulty)
    ).

% Make a move for a player
move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard) :-
    valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board),
    update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, IntermediateBoard),
    handle_removals(IntermediateBoard, Variant, UpdatedBoard),
    display_board(Size, UpdatedBoard).

% Handle blocked stones based on game variant
handle_removals(Board, base, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), StonesToRemove),
    remove_stones(StonesToRemove, Board, UpdatedBoard).

handle_removals(Board, medium_churn, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), BlockedStones),
    remove_contributing_black_stones(BlockedStones, Board, UpdatedBoard).

% Handle removals based on the "high churn" condition
handle_removals(Board, high_churn, UpdatedBoard) :-
    findall((Row, Col, Player), blocked_stone(Row, Col, Player, Board), BlockedStones),
    
    % Only remove black stones if blocked stones are found
    (   BlockedStones \= [] ->
        remove_all_black_stones(Board, TempBoard),
        remove_stones(BlockedStones, TempBoard, UpdatedBoard)
    ;   UpdatedBoard = Board
    ).

% Integration into the game loop
computer_move(Player, Size, Board, UpdatedBoard, Difficulty) :-
    (   Difficulty == easy ->
        random_computer_move(Player, Size, Board, UpdatedBoard)
    ;   Difficulty == hard ->
        greedy_computer_move(Player, Size, Board, UpdatedBoard)
    ).

% Easy mode computer move: Randomly selects a valid move
random_computer_move(Player, Size, Board, UpdatedBoard) :-
    findall(
        (FromRow, FromCol, ToRow, ToCol),
        (   member((FromRow, FromCol, Player), Board),
            between(1, Size, ToRow),
            between(1, Size, ToCol),
            (   ToRow \= FromRow; ToCol \= FromCol),
            valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board)
        ),
        Moves
    ),
    (   Moves = [] ->
        write('No valid moves available for player '), write(Player), nl,
        UpdatedBoard = Board  % No moves available
    ;   random_member((FromRow, FromCol, ToRow, ToCol), Moves),
        move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard)
    ).

% Hard mode computer move: Greedy approach to maximize immediate advantage
greedy_computer_move(Player, Size, Board, UpdatedBoard) :-
    findall(
        (FromRow, FromCol, ToRow, ToCol),
        (   member((FromRow, FromCol, Player), Board),
            between(1, Size, ToRow),
            between(1, Size, ToCol),
            (   ToRow \= FromRow; ToCol \= FromCol),
            valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board)
        ),
        Moves
    ),
    (   Moves = [] ->
        write('No valid moves available for player '), write(Player), nl,
        UpdatedBoard = Board  % No moves available
    ;
        evaluate_move(Moves, Board, BestMove),
        BestMove = (FromRow, FromCol, ToRow, ToCol),
        move(Player, FromRow, FromCol, ToRow, ToCol, Size, Board, UpdatedBoard)
    ).

% Insert an element into a sorted list
insert_sorted((FromRow, FromCol, ToRow, ToCol, Score), [], [(FromRow, FromCol, ToRow, ToCol, Score)]).
insert_sorted((FromRow, FromCol, ToRow, ToCol, Score), [(FromRow1, FromCol1, ToRow1, ToCol1, Score1)|Rest], [(FromRow, FromCol, ToRow, ToCol, Score), (FromRow1, FromCol1, ToRow1, ToCol1, Score1)|Rest]) :-
    Score >= Score1.
insert_sorted((FromRow, FromCol, ToRow, ToCol, Score), [(FromRow1, FromCol1, ToRow1, ToCol1, Score1)|Rest], [(FromRow1, FromCol1, ToRow1, ToCol1, Score1)|SortedRest]) :-
    Score < Score1,
    insert_sorted((FromRow, FromCol, ToRow, ToCol, Score), Rest, SortedRest).

% Sort a list of moves by inserting elements one by one
insertion_sort([], []).
insertion_sort([Head|Tail], Sorted) :-
    insertion_sort(Tail, SortedTail),
    insert_sorted(Head, SortedTail, Sorted).

% Evaluate the best move based on the score
evaluate_move(Moves, Board, BestMove) :-
    findall(
        (FromRow, FromCol, ToRow, ToCol, Score),
        (   member((FromRow, FromCol, ToRow, ToCol), Moves),
            evaluate_move_score(Player, FromRow, FromCol, ToRow, ToCol, Board, Score)
        ),
        ScoredMoves
    ),
    insertion_sort(ScoredMoves, SortedMoves),
    SortedMoves = [(FromRow, FromCol, ToRow, ToCol, _Score) | _Rest], 
    BestMove = (FromRow, FromCol, ToRow, ToCol).

% Comparison for sorting moves
compare_move_scores(CompareResult, (_, _, _, _, Score1), (_, _, _, _, Score2)) :-
    (   Score1 >= Score2
    ->  CompareResult = (=)
    ;   CompareResult = (<)
    ).

% Evaluate the score of a move
evaluate_move_score(Player, FromRow, FromCol, ToRow, ToCol, Board, Score) :-
    (   opponent(Player, Opponent),
        member((ToRow, ToCol, Opponent), Board)  % If the destination contains an opponent's piece, it's a good move
    ->  Score = 1
    ;   Score = 0
    ).

% Utility to determine the opponent player
opponent(r, b).
opponent(b, r).

% Switch to the next player
next_player(r, b).
next_player(b, r).

between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, X).

% Update the board after a move
update_board(Player, FromRow, FromCol, ToRow, ToCol, Board, UpdatedBoard) :-
    replace(Board, (FromRow, FromCol, Player), (FromRow, FromCol, black), TempBoard),
    append(TempBoard, [(ToRow, ToCol, Player)], UpdatedBoard).

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
        path_clear(FromRow, FromCol, DeltaRow, DeltaCol, Board).

% Valid movement directions (horizontal, vertical, or diagonal)
valid_direction(DeltaRow, DeltaCol) :-
    between(-1, 1, DeltaRow),
    between(-1, 1, DeltaCol),
    \+ (DeltaRow =:= 0, DeltaCol =:= 0).

% Check if the path is clear recursively
path_clear(Row, Col, DeltaRow, DeltaCol, Board) :-
    SignRow is sign(DeltaRow),
    SignCol is sign(DeltaCol),
    NextRow is Row + SignRow,
    NextCol is Col + SignCol,
    NewDeltaRow is DeltaRow - SignRow,
    NewDeltaCol is DeltaCol - SignCol,
    \+ member((NextRow, NextCol, _), Board),
    (NewDeltaRow =:= 0, NewDeltaCol =:= 0 -> true; path_clear(NextRow, NextCol, NewDeltaRow, NewDeltaCol, Board)).

% Check if a stone is blocked
blocked_stone(Row, Col, Player, Board) :-
    member((Row, Col, Player), Board),
    \+ (
        valid_direction(DeltaRow, DeltaCol),
        (DeltaRow \= 0; DeltaCol \= 0),
        ToRow is Row + DeltaRow,
        ToCol is Col + DeltaCol,
        valid_move(Player, Row, Col, ToRow, ToCol, Board)
    ).

% Check if a stone can move
can_move(Row, Col, ToRow, ToCol, Board) :-
    member((Row, Col, Player), Board),
    valid_direction(DeltaRow, DeltaCol),
    ToRow is Row + DeltaRow,
    ToCol is Col + DeltaCol,
    valid_move(Player, Row, Col, ToRow, ToCol, Board).

% Remove blocked stones from the board
remove_stones([], Board, Board).
remove_stones([(Row, Col, _) | Rest], Board, UpdatedBoard) :-
    member((Row, Col, _), Board), 
    delete_piece(Board, (Row, Col, _), TempBoard),
    remove_stones(Rest, TempBoard, UpdatedBoard).

% Remove contributing black stones (Medium Churn Variant)
remove_contributing_black_stones(BlockedStones, Board, UpdatedBoard) :-
    findall(
        (Row, Col),
        (member((Row, Col, black), Board),
         adjacent_to_blocked_stones((Row, Col), BlockedStones, Board)),
        ContributingBlackStones
    ),
    subtract(Board, ContributingBlackStones, TempBoard),
    subtract(TempBoard, BlockedStones, UpdatedBoard).

% Check if a black stone is adjacent to any blocked stone
adjacent_to_blocked_stones((BlackRow, BlackCol), BlockedStones, _) :-
    member((BlockedRow, BlockedCol, _), BlockedStones),
    abs(BlackRow - BlockedRow) =< 1,
    abs(BlackCol - BlockedCol) =< 1.

% Remove all black stones (High Churn Variant)
remove_all_black_stones([], []).
remove_all_black_stones([(Row, Col, Player)|Rest], UpdatedBoard) :-
    (   is_black_stone((Row, Col, Player))  % If it's a black stone, exclude it
    ->  remove_all_black_stones(Rest, UpdatedBoard)
    ;   remove_all_black_stones(Rest, UpdatedRest),  % Otherwise, keep it and continue
        UpdatedBoard = [(Row, Col, Player)|UpdatedRest]).

% Helper to identify black stones
is_black_stone((_, _, black)).

% Utility to delete an element from the board
delete_piece([Elem | Rest], Elem, Rest).
delete_piece([Other | Rest], Elem, [Other | UpdatedRest]) :-
    Other \= Elem,
    delete_piece(Rest, Elem, UpdatedRest).

% Check win condition
game_over(GameState) :-
    GameState = game_state(board(Board), current_player(CurrentPlayer), _Size),
    opponent(CurrentPlayer, Opponent),
    
    valid_moves(GameState, CurrentPlayerMoves),
    valid_moves(GameState, OpponentMoves),
    
    % Check if either player has no valid moves left
    (   CurrentPlayerMoves = [] ->
        (   OpponentMoves = [] ->
            write('Both players are immobile. It is a draw!'), nl
        ;   write('Player '), write(CurrentPlayer), write(' has no moves left. You lose!'), nl
        )
    ;   OpponentMoves = [] ->
        write('Player '), write(Opponent), write(' has no moves left. You win!'), nl
    ).

% Returns all possible valid moves
valid_moves(GameState, ListOfMoves) :-
    GameState = game_state(board(Board), current_player(CurrentPlayer), size(Size)),
    findall(
        (FromRow, FromCol, ToRow, ToCol),
        (   member((FromRow, FromCol, Player), Board),
            between(1, Size, ToRow),
            between(1, Size, ToCol),
            (   ToRow \= FromRow; ToCol \= FromCol),
            valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board)
        ),
        ListOfMoves
    ).

% Evaluate the game state for a player
value(GameState, Player, Value) :-
    GameState = game_state(board(Board), _, _),
    count_pieces(Board, Player, PlayerPieces),
    opponent(Player, Opponent),
    count_pieces(Board, Opponent, OpponentPieces),
    Value is PlayerPieces - OpponentPieces.

% Helper to count the number of pieces for a player
count_pieces(Board, Player, Count) :-
    findall((Row, Col, Player), member((Row, Col, Player), Board), Pieces),
    length(Pieces, Count).
