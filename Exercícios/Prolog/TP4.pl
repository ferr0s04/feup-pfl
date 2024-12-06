% TP4 - Cut e I/O

% Exercício 1 - Cut

s(1).
s(2):- !.
s(3).

% a) s(X). -> X=1, X=2 -> CUT

/* b) s(X), s(Y). -> X=1, Y=1
                     X=1, Y=2
                     X=2, Y=1
                     X=2, Y=2
                     CUT
*/

/* c) s(X), !, s(Y). -> X=1, Y=1
                        X=1, Y=2
                        CUT
*/


% Exercício 2 - Efeitos de um Cut
data(one).
data(two).
data(three).
cut_test_a(X):- data(X).
cut_test_a('five').
cut_test_b(X):- data(X), !.
cut_test_b('five').
cut_test_c(X, Y):- data(X), !, data(Y).
cut_test_c('five', 'five').

% a) cut_test_a(X), write(X), nl, fail. -> one two three five -> no
% b) cut_test_b(X), write(X), nl, fail. -> one -> no
% c) cut_test_c(X, Y), write(X-Y), nl, fail. -> one-one one-two one-three -> no


% Exercício 3 - Cuts Vermelhos e Verdes
immature(X):- adult(X), !, fail. % Cut vermelho -> força a falha, alterando a lógica do programa
immature(_X).
adult(X):- person(X), !, age(X, N), N >=18. % Cuts verdes. Impede a avaliação das expressões seguintes mas não altera o resultado, logo aumenta a eficiência
adult(X):- turtle(X), !, age(X, N), N >=50.
adult(X):- spider(X), !, age(X, N), N>=1.
adult(X):- bat(X), !, age(X, N), N >=5.

% Cut vermelho -> influencia os resultados
% Cut verde -> não influencia os resultados mas aumenta a eficiência


% Exercício 4 - Valor Máximo
max(A, B, C, Max) :-
    A >= B, A >= C, !, Max = A.
max(_, B, C, Max) :-
    B >= C, !, Max = B.
max(_, _, C, C).


% Exercício 5 - I/O de Dados
% a)

% b)

% d)


% Exercício 6 - Imprimir Listas
% a)

% c)

% d)

% e)
