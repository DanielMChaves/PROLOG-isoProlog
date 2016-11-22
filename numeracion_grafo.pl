:- module(_,_).

% Metodologia:
%
% 1º Creamos una lista de los nodos
% 2º Obtenemos las permutaciones posibles de la lista
%    de nodos creada antes.
% 3º Numeramos los nodos de mayor a menor
% 4º Numeramos los arcos, si no respentan la restriccion,
%    por backtraking volvemos al paso 2º para volver a
%    intentarlo con otra permutacion.

% enumerar/3: Predicado que enumera los arcos y los vertices a
% partir de una lista de Arcos que se pasa en el primer parametro.
enumerar([],[],[]) :- !.
enumerar(Arcos,EnumNodos,EnumArcos) :-
	crearListaNodos(Arcos,ListaNodosRepetidos),
	setof(X,member(X,ListaNodosRepetidos),ListaNodos),
	list_length(ListaNodos,NumeroNodos),
	permutaciones(ListaNodos,ListaNodosPermutada),
	enumerarNodos(ListaNodosPermutada,NumeroNodos,EnumNodos),
	NumeroArcos is NumeroNodos - 1,
	crearListaNumeros(NumeroArcos,ListaNumeros),
	enumerarArcos(Arcos,EnumNodos,ListaNumeros,EnumArcos).

% crearListaNodos/2: Predicado que crea una lista de nodos a partir
% de una lista de arcos.
crearListaNodos([X-X1|[]],[Y,Ys]) :-
	Y = X,
	Ys = X1.
crearListaNodos([X-X1|Xs],[Y,Y1|Ys]) :-
	Y = X,
	Y1 = X1,
	crearListaNodos(Xs,Ys).

% permutaciones/2: Predicado que permuta una lista.
permutaciones([X|Y],Z) :- 
	permutaciones(Y,W), 
	permutacionesAux(X,Z,W).  
permutaciones([],[]).

% enumerarNodos/2: Predicado que crea una lista de nodos enumerados
% a partir de una lista de nodos.
enumerarNodos([],0,[]).
enumerarNodos([X|Xs],Enum,[Y|Ys]) :-
	Y = enum(Enum,X),
	EnumNuevo is Enum - 1,
	enumerarNodos(Xs,EnumNuevo,Ys).

% crearListaNumeros/2: Predicado que crea una lista de numeros.
crearListaNumeros(0,[]) :- !.
crearListaNumeros(N,[X|Xs]) :-
	X = N,
	Nf is N - 1,
	crearListaNumeros(Nf,Xs).

% enumerarArcos/4: Predicado que crea una lista de arcos numerados
% a partir de una lista de arcos y nodos enumerados.
enumerarArcos([],_,_,[]).
enumerarArcos([X1-X2|Xs],EnumNodos,ListaNumeros,[Z|Zs]) :-
	buscarEnum(EnumNodos,X1,Enum1),
	buscarEnum(EnumNodos,X2,Enum2),
	abs(Enum1,Enum2,EnumArco),
	eliminarNumero(ListaNumeros,EnumArco,ListaNumerosNueva),
	Z = enum(EnumArco,X1-X2),
	enumerarArcos(Xs,EnumNodos,ListaNumerosNueva,Zs).
	
% buscarEnum/3: Predicado que busca la enumeracion de un nodo.
buscarEnum([],_,_) :- fail.
buscarEnum([enum(Enum,E)|_],E,Enum).
buscarEnum([enum(_,_)|Xs],E,Enum) :-
	buscarEnum(Xs,E,Enum).
	
% eliminarNumero/3: Predicado que elimina un numero de una
% lista de numeros
eliminarNumero([X|Xs],X,Xs).
eliminarNumero([Y|Xs],X,[Y|Ys]):-
	eliminarNumero(Xs,X,Ys).
	
% PREDICADOS AUXILIARES

% permutacionesAux/3: Predicado auxiliar del predicado permutaciones/2
permutacionesAux(X,[X|R],R).  
permutacionesAux(X,[F|R],[F|S]) :- 
	X \= F,
	permutacionesAux(X,R,S).

% list_length/2: Predicado que devuelve el tamano de una lista
list_length([],0) :- !.
list_length([_|Xs],L) :- 
	list_length(Xs,N), 
	L is N + 1.

% abs/3: Predicado que realiza el valor absoluto de un numero
abs(X,Y,Z) :- 
	X >= Y, 
	Z is X - Y.
abs(X,Y,Z) :- 
	X < Y,
	Z is Y - X.

% Ejemplo:
% ?- enumerar([a-b,b-c],EnumNodos,EnumArcos).
% EnumNodos = [enum(3,a), enum(1,b), enum(2,c)],
% EnumArcos = [enum(2,a-b), enum(1,b-c)]
