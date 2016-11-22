:- module(_,_).

% Metodologia:
%
% 1º Ordenamos la lista de buzos de mayor a menor tiempo
% 2º Obtenemos la lista de todas las permutaciones posibles
%    de la lista de buzos.
% 3º Le ponemos un peso a cada pareja, que este peso es la
%    resta en valor absoluto del tiempo de los miembro de
%    la pareja.
% 4º Recursivo:
%    - Cogemos el buzo mas lento y vamos trabajando con las
%      parejas de mayor a menos peso.
%    - Cuando el buzo haya acabado pasa a trabajar la pareja
%      con mayor peso del buzo que no ha terminado de trabajar.

% reparacion/3: Predicado que ordena las parejas de buzo para que
% reparen el barco antes de que se acabe el tiempo
reparacion([],_,[]).
reparacion(Equipo,Tiempo,OrdenParejas) :-
	ordenarBuzos(Equipo,EquipoOrdenado),
	permutaciones(EquipoOrdenado,ListaPermutaciones),
	introducirPeso(ListaPermutaciones,ListaConPesos),
	ordenDeTrabajo(ListaConPesos,ListaDeTrabajo),
	listaDeBuzos(ListaDeTrabajo,ListaDeBuzos),
	reparacionAux(ListaDeBuzos,Tiempo,0,OrdenParejas),!.

% ordenarBuzos/2: Predicado que ordena la lista de buzos de mayor a
% menor segun su tiempo de trabajo.
ordenarBuzos(Equipo,EquipoOrdenado) :-
	voltearBuzos(Equipo,EquipoVolteado),
	setof(X,member(X,EquipoVolteado),EquipoVolteadoInverso),
	inversa(EquipoVolteadoInverso,EquipoVolteadoAux),
	voltearBuzos(EquipoVolteadoAux,EquipoOrdenado).

% voltearBuzos/2: Predicado que cambia de orden los argumentos
% del predicado buzo.
voltearBuzos([],[]).
voltearBuzos([buzo(N,T)|Xs],[buzo(T,N)|Ys]) :-
	voltearBuzos(Xs,Ys).

% permutaciones/2: Predicado que de una lista devuelve
% todas las permutaciones posibles de dicha lista.
permutaciones([],[]).
permutaciones([X|Xs],F) :-
	cartesiano(X,Xs,Z),
	permutaciones(Xs,Zs),
	concat(Z,Zs,F).

% introducirPeso/2: Predicado que pone el peso de
% de cada pareja
introducirPeso([],[]).
introducirPeso([[buzo(N1,T1),buzo(N2,T2)]|Xs],F) :-
	P is abs(T1 - T2),
	Y = [P,buzo(N1,T1),buzo(N2,T2)],
	introducirPeso(Xs,Ys),
	concat([Y],Ys,F).

% ordenDeTrabajo/2: Predicado que de una lista de parejas
% con peso devuelve la lista del orden de trabajo de cada
% pareja con el peso de cada pareja.
ordenDeTrabajo([],[]).
ordenDeTrabajo(ListaConPesos,F) :-
	masLento(ListaConPesos,buzo(_,0),MasLento),
	parejasLento(ListaConPesos,MasLento,ListaMasLentoMal),
	setof(X,member(X,ListaMasLentoMal),ListaMasLentoInv),
	inversa(ListaMasLentoInv,X),
	borrarLista(ListaConPesos,X,Xs),
	ordenDeTrabajo(Xs,Xf),
	concat(X,Xf,F).

% listaDeBuzos/2: Predicado que da una lista del orden en el
% que van a trabajar los buzos a partir de la lista de parejas
% de buzos con pesos.
listaDeBuzos([[P,buzo(N1,T1),buzo(N2,T2)]|Xs],[F|Fs]) :-
	 F = buzo(N1,T1),
	 listaDeBuzosAux([[P,buzo(N1,T1),buzo(N2,T2)]|Xs],Fs).
listaDeBuzosAux([],[]).
listaDeBuzosAux([[_,buzo(_,_),buzo(N2,T2)]|Xs],[F|Fs]) :-
	 F = buzo(N2,T2),
	 listaDeBuzosAux(Xs,Fs).

% reparacionAux/4: Predicado que se encarga de la gestion de la
% reparacion del barco. Para mas detalles, este predicado esta
% explicado en mas profundidad al principio del fichero en el 3º punto
reparacionAux([],TB,0,[]) :-
	TB > 0.
reparacionAux([buzo(_,_)],TB,0,[]) :-
	TB > 0.
reparacionAux([buzo(_,T1),buzo(_,T2)|_],0,_,[]) :-
	T1 > 0,
	T2 > 0,
	!,fail.
reparacionAux([buzo(N1,0),buzo(N2,T2)|[]],0,TP,[Y|[]]) :-
	T2 > 0,
	Y = pareja(N1,N2,TP).
reparacionAux([buzo(N1,0),buzo(N2,0)|[]],0,TP,[Y|[]]) :-
	Y = pareja(N1,N2,TP).
% Caso 0 - X
reparacionAux([buzo(N1,0),buzo(N2,T2)|Xs],TB,TP,[Y|Ys]) :-
	T2 > 0,
	TB > 0,
	Y = pareja(N1,N2,TP),
	reparacionAux([buzo(N2,T2)|Xs],TB,0,Ys).
% Caso x - 0
reparacionAux([buzo(N1,T1),buzo(N2,0)|Xs],TB,TP,[Y|Ys]) :-
	T1 > 0,
	TB > 0,
	Y = pareja(N1,N2,TP),
	reparacionAux([buzo(N1,T1)|Xs],TB,0,Ys).
% Caso 0 - 0
reparacionAux([buzo(N1,0),buzo(N2,0)|Xs],TB,TP,[Y|Ys]) :-
	TB > 0,
	Y = pareja(N1,N2,TP),
	reparacionAux(Xs,TB,0,Ys).
reparacionAux([buzo(N1,T1),buzo(N2,T2)|Xs],TB,TP,[Y|Ys]) :-
	T1 > 0,
	T2 > 0,
	TB > 0,
	T11 is T1 - 1,
	T22 is T2 - 1,
	TP1 is TP + 1,
	TB1 is TB - 1,
	reparacionAux([buzo(N1,T11),buzo(N2,T22)|Xs],TB1,TP1,[Y|Ys]).

% masLento/3: Predicado que de una lista de buzos
% nos devuelve el buzo que mas tiempo necesita
% para reparar el barco, es decir, el buzo mas lento.
masLento([],X,X).
masLento([[_,buzo(N1,T),_]|Xs],buzo(_,I),F) :-
	T > I,
	masLento(Xs,buzo(N1,T),F).
masLento([[_,buzo(_,T),_]|Xs],buzo(N2,I),F) :-
	T =< I,
	masLento(Xs,buzo(N2,I),F).

% parejasLento/3: Predicado que de crea una lista
% con las parejas del buzo mas lento.
parejasLento([],_,[]).
parejasLento([[P,X,Cp]|Xs],X,[[P,X,Cp]|Ys]) :-
	parejasLento(Xs,X,Ys).
parejasLento([[_,_,_]|Xs],X1,Ys) :-
	parejasLento(Xs,X1,Ys).

% borrarLista/3: Predicado que borra los elementos de
% una lista.
borrarLista([],_,[]).
borrarLista([X|Xs],Y,Z) :-
	member(X,Y),
	borrarLista(Xs,Y,Z), !.
borrarLista([X|Xs],Y,[X|Zs]) :-
	borrarLista(Xs,Y,Zs).

% PREDICADOS AUXILIARES

% cartesiano/3: Predicado que realiza el producto cartesiano.
cartesiano(X,Y,Z) :-
	findall([X,Eb],member(Eb,Y),Z).

% inversa/2: Predicado que le da la vuelta a una lista.
inversa([],[]).
inversa([H|T],L):-
	inversa(T,R),
	concat(R,[H],L).

% concat/3: Predicado que concatena dos listas.
concat([],L,L).
concat([X|L1],L2,[X|L3]):-
	concat(L1,L2,L3).

% Ejemplo:
% ?- reparacion([buzo(gomez,45),buzo(lopez,20),buzo(garcia,40),buzo(perez,15)],80,OrdenParejas).
