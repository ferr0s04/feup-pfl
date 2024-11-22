% TP2 - Recursão em Prolog

% Exercício 1 - Backtracking
r(a, b).
r(a, d).
r(b, a).
r(a, c).
s(b, c).
s(b, d).
s(c, c).
s(d, e).

/*
r(X, Y), s(Y, Z). -> r(a, b), s(b, c) encontrado

s(Y, Y), r(X, Y). -> s(c, c), r(a, c) -> encontrado

r(X, Y), s(Y, Y). -> r(a, b), s(b, b) não encontrado
                  -> r(a, d), s(d, d) não encontrado
                  -> r(b, a), s(a, a) não encontrado
                  -> r(a, c), s(c, c) encontrado
                  3 backtrackings
*/


% Exercício 2 - Backtracking e Search Tree
pairs(X, Y):- d(X), q(Y).
pairs(X, X):- u(X).
u(1).
d(2).
d(4).
q(4).
q(16).

% a) Árvore de pesquisa para pairs(X, Y).
/*
d(2), q(4) -> X = 2 e Y = 4
d(2), q(16) -> X = 2 e Y = 16
d(4), q(4) -> X = 4 e Y = 4
d(4), q(16) -> X = 4 e Y = 16
*/


% Exercício 4 - Recursão
% a) Fatorial
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% b) Soma dos números de 1 a N
sum_rec(0, 0).
sum_rec(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum_rec(N1, Sum1),
    Sum is N + Sum1.

% c) Potência de um número
pow_rec(_, 0, 1).
pow_rec(X, Y, P) :-
    Y1 is Y - 1,
    pow_rec(X, Y1, P1),
    P is P1 * X.

% d) Quadrado de um número (sem multiplicações)
% Dica: usar a relação "n^2 = (n - 1)^2 - 1 + n + n"
square_rec(0, 0).
square_rec(N, S) :- 
    N1 is N - 1,
    square_rec(N1, S1),
    S is S1 - 1 + N + N.

% e) Sequência de Fibonacci
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 0,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Exercício 5 - Tail Recursiva
% Fatorial
factorial_tail(N, F) :-
    factorialh(N, F, 1).

factorialh(0, F, F).
factorialh(N, F, A) :-
    N1 is N - 1,
    A1 is A * N,
    factorialh(N1, F, A1).

% Soma dos números de 1 a N
sum_rec_tail(N, Sum) :-
    sum_rech(N, Sum, 0).

sum_rech(0, Sum, Sum).
sum_rech(N, Sum, A) :-
    N1 is N - 1,
    A1 is A + N,
    sum_rech(N1, Sum, A1).

% Potência de um número
pow_rec_tail(X, Y, P) :-
    pow_rech(X, Y, P, 1).

pow_rech(_, 0, P, P).
pow_rech(X, Y, P, A) :-
    Y1 is Y - 1,
    A1 is A * X,
    pow_rech(X, Y1, P, A1).

% Quadrado de um número (sem multiplicações)
square_rec_tail(N, S) :-
    square_rech(N, S, 0).

square_rech(0, S, S).
square_rech(N, S, A) :-
    N1 is N - 1,
    A1 is A - 1 + N + N,
    square_rech(N1, S, A1).

% Sequência de Fibonacci
fibonacci_tail(N, F) :-
    fibonaccih(N, F, 1, 1).

fibonaccih(0, F, F, _).
fibonaccih(1, F, _, F).
fibonaccih(N, F, A1, A2) :-
    N1 is N - 1,
    A1N is A1 + A2,
    A2N is A1,
    fibonaccih(N1, F, A1N, A2N).

% Exercício 7 - Relações de Parentesco
