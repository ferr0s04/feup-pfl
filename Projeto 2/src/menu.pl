% Main menu
menu :- 
    nl,
    write('======================='), nl,
    write('      BLACKSTONE       '), nl,
    write('======================='), nl,
    write('1. Play'), nl,
    write('2. Help'), nl,
    write('3. Exit'), nl,
    write('Choose an option (1-3): '),
    read(Choice),
    handle_menu_choice(Choice).

% Handle menu choices
handle_menu_choice(1) :- play_menu.
handle_menu_choice(2) :- show_help, menu.
handle_menu_choice(3) :- write('Goodbye!'), nl, halt.
handle_menu_choice(_) :- write('Invalid choice. Please try again.'), nl, menu.

% Play menu
play_menu :- 
    nl, 
    write('Select Board Size:'), nl,
    write('1. 6x6'), nl,
    write('2. 8x8'), nl,
    write('3. 10x10'), nl,
    write('4. 12x12'), nl,
    write('Choose an option (1-4): '),
    read(BoardSizeOption),
    (   integer(BoardSizeOption), board_size(BoardSizeOption, Size) -> 
        write('Select Game Type:'), nl, 
        write('1. Player vs Player'), nl, 
        write('2. Player vs Computer'), nl, 
        write('3. Computer vs Computer'), nl, 
        write('Choose an option (1-3): '),
        read(GameTypeOption),
        (   integer(GameTypeOption), game_type(GameTypeOption, GameType) -> 
            nl, 
            write('Starting game...'), nl, 
            start_game(Size, GameType) 
        ;   write('Invalid game type. Returning to main menu.'), nl, 
            menu 
        )
    ;   write('Invalid board size. Returning to main menu.'), nl, 
        menu
    ).

% Map board size option to size
board_size(1, 6).
board_size(2, 8).
board_size(3, 10).
board_size(4, 12).
board_size(_, 6) :- write('Invalid choice. Defaulting to 6x6.'), nl.

% Map game type option to type
game_type(1, pvp).
game_type(2, pvc).
game_type(3, cvc).
game_type(_, pvp) :- write('Invalid choice. Defaulting to Player vs Player.'), nl.

% Show help message
show_help :- 
    nl, 
    write('How to Play:'), nl, 
    write('1. Players take turns making moves.'), nl, 
    write('2. Enter moves in the format [FromRow, FromCol, ToRow, ToCol].'), nl, 
    write('3. The game ends when a player has no valid moves left.'), nl, 
    write('4. In Player vs Computer mode, the computer will make moves automatically.'), nl, 
    nl.
