:-dynamic saga/4, movie/8.

%saga(SagaID, Saga Name, Number of Movies in Saga, Creator)
saga(1, 'Jurassic Park',  6, 'Michael Crichton').
saga(2, 'Indiana Jones',  4, 'George Lucas').
saga(3, 'Star Wars',      9, 'George Lucas').
saga(4, 'Harry Potter',   0, 'J. K. Rowling').
saga(6, 'Jaws',           0, 'Peter Benchley').

%movie(Movie Title, Year of Release, SagaID, Duration, IMDB Score, Director, Composer, Cast)
movie('Jurassic Park',                  1993, 1, 127, 8.2, 'Steven Spielberg', 'John Williams',     ['Sam Neill', 'Jeff Goldblum', 'Laura Dern', 'BD Wong']).
movie('The Lost World: Jurassic Park',  1997, 1, 129, 6.5, 'Steven Spielberg', 'John Williams',     ['Jeff Goldblum', 'Julianne Moore', 'Vince Vaughn', 'Richard Schiff']).
movie('Jurassic Park III',              2001, 1,  92, 5.9, 'Joe Johnston',     'Don Davis',         ['Sam Neill', 'William H. Macy', 'Téa Leoni']).
movie('Jurassic World',                 2015, 1, 124, 6.9, 'Colin Trevorrow',  'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'Irrfan Khan', 'BD Wong']).
movie('Jurassic World: Fallen Kingdom', 2018, 1, 128, 6.1, 'J.A. Bayona',      'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'James Cromwell', 'BD Wong']).
movie('Jurassic World: Dominion',       2022, 1, 147, 5.6, 'Colin Trevorrow',  'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'Campbell Scott', 'BD Wong']).

movie('Raiders of the Lost Ark',       1981, 2, 115, 8.4, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Karen Allen', 'John Rhys-Davies']).
movie('The Temple of Doom',            1984, 2, 118, 7.5, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Kate Capshaw', 'Ke Huy Quan']).
movie('The Last Crusade',              1989, 2, 127, 8.2, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Alison Doody', 'Sean Connery']).
movie('Kingdom of the Crystal Skull',  2008, 2, 122, 6.2, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Karen Allen', 'Shia LaBeouf']).

movie('The Phantom Menace',       1999, 3, 136, 6.5, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Liam Neeson', 'Natalie Portman', 'Ian McDiarmid']).
movie('Attack of the Clones',     2002, 3, 142, 6.6, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Hayden Christensen', 'Natalie Portman', 'Christopher Lee']).
movie('Revenge of the Sith',      2005, 3, 140, 7.6, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Hayden Christensen', 'Natalie Portman', 'Christopher Lee']).
movie('A New Hope',               1977, 3, 121, 8.6, 'George Lucas',     'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Alec Guinness']).
movie('The Empire Strikes Back',  1980, 3, 124, 8.7, 'Irvin Kershner',   'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Billy Dee Williams']).
movie('Return of the Jedi',       1983, 3, 131, 8.3, 'Richard Marquand', 'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Ian McDiarmid']).
movie('The Force Awakens',        2015, 3, 138, 7.8, 'J. J. Abrams',     'John Williams', ['Daisy Ridley', 'Harrison Ford', 'Mark Hamill', 'Carrie Fisher']).
movie('The Last Jedi',            2017, 3, 152, 6.9, 'Rian Johnson',     'John Williams', ['Daisy Ridley', 'Mark Hamill', 'Carrie Fisher', 'John Boyega']).
movie('The Rise of Skywalker',    2019, 3, 141, 6.4, 'J. J. Abrams',     'John Williams', ['Daisy Ridley', 'Mark Hamill', 'John Boyega', 'Adam Driver']).

% Exercício 1: check if two movies have the same composer
same_composer(Movie1, Movie2) :-
    movie(Movie1, _, _, _, _, _, Composer, _),
    movie(Movie2, _, _, _, _, _, Composer, _),
    Movie1 \= Movie2.

% Exercício 2: find the saga name for a movie
movie_from_saga(Movie, Saga) :-
    movie(Movie, _, SagaID, _, _, _, _, _),
    saga(SagaID, Saga Name, _, _),
    Saga = Saga Name.

% Exercício 3: find the most recent movie in a saga
saga_most_recent_movie(Saga, Movie) :-
	saga(SagaID, Saga, _, _),
	movie(Movie, Year, SagaID, _, _, _, _, _),
    \+ (movie(_, OtherYear, SagaID, _, _, _, _, _), OtherYear > Year).

% Exercício 4: add a movie to a saga
add_movie_to_saga(Saga, Movie, Year, Duration, Score, Director, Composer, Cast) :-
    \+ movie(Movie, Year, SagaID, Duration, Score, Director, Composer, Cast),
    assert(movie(Movie, Year, SagaID, Duration, Score, Director, Composer, Cast)),
    saga(SagaID, Saga, NumMovies, Creator),
    NewNumMovies is NumMovies + 1,
    retract(saga(SagaID, Saga, NumMovies, Creator)),
    assert(saga(SagaID, Saga, NewNumMovies, Creator)).

% Exercício 6: list of all actors in a saga
saga_cast(Saga, Cast) :-
    saga(SagaID, Saga, _, _),
    setof(Actor, movie(_, _, SagaID, _, _, _, _, Actors), member(Actor, Actors), Cast).

% Exercício 7: display actors in odd positions from the previous list
sample_cast(Saga, Cast) :-
    saga_cast(Saga, FullCast),
    odd_positions(FullCast, Cast).

odd_positions([], []).
odd_positions([A], [A]).
odd_positions([A, _|T], [A|Rest]) :-
    odd_positions(T, Rest).

% Exercício 8: average score for a composer's movies
composer_rating(Composer, AvgScore) :-
	findall(Score, movie(_, _, _, _, Score, _, Composer, _), Scores),
	sumlist(Scores, SumScores),
	length(Scores, NumScores),
	AvgScore is SumScores / NumScores.