% TP3 - Listas

% Exercício 1 - Igualdades com Listas
% a) [a | [b, c, d] ] = [a, b, c, d]                -> Yes
% b) | ?- [a | b, c, d ] = [a, b, c, d]             -> Erro de sintaxe (o fim de uma lista tem de ser uma lista)
% c) | ?- [a | [b | [c, d] ] ] = [a, b, c, d]       -> Yes
% d) | ?- [H|T] = [pfl, lbaw, fsi, ipc]             -> H = pfl, T = [lbaw, fsi, ipc]
% e) | ?- [H|T] = [lbaw, ltw]                       -> H = lbaw, T = [ltw]
% f) | ?- [H|T] = [leic]                            -> H = leic, T = []
% g) | ?- [H|T] = []                                -> No
% h) | ?- [H|T] = [leic, [pfl, ipc, lbaw, fsi] ]    -> H = leic, T = [[pfl, ipc, lbaw, fsi]]
% i) | ?- [H|T] = [leic, Two]                       -> H = leic, T = [Two]
% j) | ?- [Inst, feup] = [gram, LEIC]               -> Inst = gram, LEIC = feup
% k) | ?- [One, Two | Tail] = [1, 2, 3, 4]          -> One = 1, Two = 2, Tail = [3, 4]
% l) | ?- [One, Two | Tail] = [leic | Rest]         -> One = leic, Rest = [Two | Tail]


% Exercício 2 - Recursão com Listas
% a) Tamanho da lista
list_size([], 0).
list_size([_ | T], Size) :-
    list_size(T, SizeT),
    Size is SizeT + 1.

% b) Soma dos números de uma lista
list_sum([], 0).
list_sum([H | T], Sum) :-
    list_sum(T, SumT),
    Sum is SumT + H.

% d) Produto dos números de uma lista
list_prod([], 1).
list_prod([H | T], Prod) :-
    list_prod(T, ProdT),
    Prod is ProdT * H.

% e) Número de ocorrências de um elemento numa lista
count(_, [], 0). % Para listas vazias
count(Elem, [Elem | T], N) :- % Se Head = Elem
    count(Elem, T, N1),
    N is N1 + 1. % Somar 1 à contagem
count(Elem, [_ | T], N) :- % Senão, continuar com a Tail
    count(Elem, T, N).


% Exercício 3 - Manipulação de Listas
% g) - Lista de repetições de um elemento
replicate(0, _, []).
replicate(Amount, Elem, [Elem | T]) :-
    replicate(Amount1, Elem, T),
    Amount is Amount1 + 1.

% i) - Inserir um elemento numa determinada posição
insert_elem(0, [H | T], A, [A | [H | T]]). % Caso base: se o índice for 0
insert_elem(Index, [H | T], A, [H | T1]) :-
    Index > 0,
    insert_elem(Index1, T, A, T1),
    Index is Index1 + 1.

% insert_elem(2, [1, 2, 3, 4, 5], 10, List2) -> List2 = [1,2,10,3,4,5] 

% j) - Eliminar um elemento de uma determinada posição
delete_elem(0, [H | T], H, T). % Caso base: se o índice for 0
delete_elem(Index, [H | T], A, [H | T1]) :-
    Index > 0,
    delete_elem(Index1, T, A, T1),
    Index is Index1 + 1.


% Exercício 4 - Append
% a) Concatenar duas listas numa terceira
list_append([], L2, L2).
list_append([H | T], L2, [H | T3]) :-
    list_append(T, L2, T3).

% b) Se um elemento pertence à lista, usando append
list_member(Elem, List) :-
    append(_, [Elem | _], List).

% list_member(2, [1, 2, 3, 4, 5]) -> yes
% list_member(6, [1, 2, 3, 4, 5]) -> no

% d) Se o elemento está no índice N da lista, usando append e length
list_nth(N, List, Elem) :-
    length(Prefix, N),
    append(Prefix, [Elem | _], List).

% list_nth(2, [1, 2, 3, 4, 5], 3) -> yes

% f) Eliminar uma ocorrência de um elemento da lista, usando append
list_del(List, Elem, Res) :-
    append(Prefix, [Elem | Suffix], List),
    append(Prefix, Suffix, Res).

% j) Extrair uma parte de uma lista, usando append e length


% Exercício 5 - Listas de Números
% a)

% f)


% Exercício 7 - Ordenação de Listas
% a)

% b)

% c)
