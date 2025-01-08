:- use_module(library(lists)).

% TP5 - Várias Soluções, Grafos e Pesquisa

% Exercício 3 - Horários de Aulas
%class(Course, ClassType, DayOfWeek, Time, Duration)
class(pfl, t, '2 Tue', 15, 2).
class(pfl, tp, '2 Tue', 10.5, 2).
class(lbaw, t, '3 Wed', 10.5, 2).
class(lbaw, tp, '3 Wed', 8.5, 2).
class(ipc, t, '4 Thu', 14.5, 1.5).
class(ipc, tp, '4 Thu', 16, 1.5).
class(fsi, t, '1 Mon', 10.5, 2).
class(fsi, tp, '5 Fri', 8.5, 2).
class(rc, t, '5 Fri', 10.5, 2).
class(rc, tp, '1 Mon', 8.5, 2).

% a) Verificar se duas cadeiras têm aulas no mesmo dia
same_day(Course1, Course2) :-
    class(Course1, _, Day, _, _),
    class(Course2, _, Day, _, _).

% b) Lista de cadeiras para um dia
daily_courses(Day, Courses) :-
    setof(Course, class(Course, _, Day, _, _), Courses).

% c) Lista de aulas com duração inferior a 2 horas
short_classes(L) :-
    findall(Course-Day/Time, (class(Course, _, Day, Time, Duration), 2 > Duration), L).

% short_classes(L).
% L = [ipc-'4 Thu'/14.5,ipc-'4 Thu'/16]

% d) Lista de aulas de uma cadeira
course_classes(Course, Classes) :-
    findall(Day-Time/Type, class(Course, Type, Day, Time, _), Classes).

% course_classes(pfl, Classes).
% Classes = ['2 Tue'-15/t,'2 Tue'-10.5/tp]

% e) Lista das cadeiras, sem repetições
courses(L) :-
    findall(Course, class(Course, _, _, _, _), Courses),
    sort(Courses, L).


% Exercício 4 - Voos
%flight(origin, destination, company, code, hour, duration)
flight(porto, lisbon, tap, tp1949, 1615, 60).
flight(lisbon, madrid, tap, tp1018, 1805, 75).
flight(lisbon, paris, tap, tp440, 1810, 150).
flight(lisbon, london, tap, tp1366, 1955, 165).
flight(london, lisbon, tap, tp1361, 1630, 160).
flight(porto, madrid, iberia, ib3095, 1640, 80).
flight(madrid, porto, iberia, ib3094, 1545, 80).
flight(madrid, lisbon, iberia, ib3106, 1945, 80).
flight(madrid, paris, iberia, ib3444, 1640, 125).
flight(madrid, london, iberia, ib3166, 1550, 145).
flight(london, madrid, iberia, ib3163, 1030, 140).
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165).

% a) Lista de aeroportos
get_all_nodes(ListOfAirports) :-
    findall(Airport, flight(Airport, _, _, _, _, _), L1),
    findall(Airport, flight(_, Airport, _, _, _, _), L2),
    append(L1, L2, L3),
    sort(L3, ListOfAirports).

% b) Companhia(s) aérea(s) que voam de/para mais cidades
most_diversified(CompanyCounts) :-
    setof(Count-Company, city_count(Company, Count), CompanyCounts),
    reverse(CompanyCounts, [_-Company | _]).

% Função auxiliar que conta o nº de cidades de uma companhia
city_count(Company, Count) :-
    findall(City, flight(City, _, Company, _, _, _), L1),
    findall(City, flight(_, City, Company, _, _, _), L2),
    append(L1, L2, L3),
    sort(L3, CityList),
    length(CityList, Count).

