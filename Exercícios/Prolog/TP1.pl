% TP1 - Introdução

% Exercício 3 - Red Bull Air Race
/*
• Lamb, Besenyei, Chambliss, MacLean, Mangold, Jones and Bonhomme are pilots;
• Lamb is from team Breitling; Besenyei and Chambliss from team Red Bull; MacLean
from Mediterranean Racing Team; Mangold from team Cobra; and Jones and
Bonhomme from team Matador;
• Lamb’s pilots an MX2; Besenyei, Chambliss, MacLean, Mangold, Jones and Bonhomme
all pilot an Edge540;
• Istanbul, Budapest and Porto are circuits;
• Jones won in Porto; Mangold won in Budapest and in Istanbul;
• Istanbul has 9 gates; Budapest has 6 gates; Porto has 5 gates;
• A team wins a race when one of its pilots wins that race.
*/

% a) Representação dos Dados
% pilot(PilotID, Pilot, Team, Plane)
pilot(1, 'Lamb', 'Breitling', 'MX2').
pilot(2, 'Besenyei', 'Red Bull', 'Edge540').
pilot(3, 'Chambliss', 'Red Bull', 'Edge540').
pilot(4, 'MacLean', 'Mediterranean Racing Team', 'Edge540').
pilot(5, 'Mangold', 'Cobra', 'Edge540').
pilot(6, 'Jones', 'Matador', 'Edge540').
pilot(7, 'Bonhomme', 'Matador', 'Edge540').

% circuit(Circuit, NumGates, Winner)
circuit(istanbul, 9, 5).
circuit(budapest, 6, 5).
circuit(porto, 5, 6).

% b) Questões
% i) Quem venceu no Porto?
circuit_winner(Circuit, Winner) :-
    pilot(PilotID, Winner, _, _),
    circuit(Circuit, _, PilotID).
% circuit_winner(porto, Winner) -> Winner = 'Jones'

% ii) Que equipa venceu no Porto?
circuit_team_winner(Circuit, Team) :-
    circuit_winner(Circuit, Pilot),
    pilot(_, Pilot, Team, _).
% circuit_team_winner(porto, Team) -> Team = 'Matador'

% iii) Circuitos com 9 gates
circuits_with_gates(Gates, Circuits) :-
    findall(Circuit, circuit(Circuit, Gates, _), Circuits).
% circuits_with_gates(9, Circuits) -> Circuits = [istanbul]

% iv) Pilotos que não pilotam o Edge540
pilots_without_plane(Plane, Pilots) :-
    findall(Pilot, (pilot(_, Pilot, _, OtherPlane), OtherPlane \= Plane), Pilots).
% pilots_without_plane('Edge540', Pilots) -> Pilots = ['Lamb']

% v) Pilotos que ganharam mais de 1 circuito
multiple_wins(Pilots) :-
    findall(Pilot, (pilot(PilotID, Pilot, _, _), pilot_wins(PilotID, NumWins), NumWins > 1), Pilots).

pilot_wins(PilotID, NumWins) :-
    findall(Pilot, (circuit(_, _, Winner), pilot(PilotID, Pilot, _, _), Winner == PilotID), Wins),
    length(Wins, NumWins).
% multiple_wins(Pilots) -> Pilots = ['Mangold']

% vi) Avião pilotado pelo vencedor no Porto
winner_plane(Circuit, Plane) :-
    circuit(Circuit, _, PilotID),
    pilot(PilotID, _, _, Plane).
% winner_plane(porto, Plane) -> Plane = 'Edge540'


% Exercício 4 - Erros de Programação
/*
translate(Code, Meaning):-
Code = 1,
Meaning = ‘Integer Overflow’.
translate(Code, Meaning):-
Code = 2,
Meaning = ‘Division by zero’.
translate(Code, Meaning):-
Code = 3,
Meaning = ‘ID Unknown’.
*/

% Forma correta em Prolog:
translate(1, 'Integer Overflow').
translate(2, 'Division by zero').
translate(3, 'ID Unknown').
