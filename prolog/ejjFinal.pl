% Definición de un árbol utilizando listas anidadas
arbol([]).

/* ------------------------------------------------------------------------------------------------- */

% Predicado para añadir un nodo a un árbol
anadir_nodo(Valor, [], [Valor, [], []]).

anadir_nodo(Valor, [ValorActual, Izquierdo, Derecho], [ValorActual, NuevoIzquierdo, Derecho]) :-
  Valor < ValorActual,
  anadir_nodo(Valor, Izquierdo, NuevoIzquierdo).

anadir_nodo(Valor, [ValorActual, Izquierdo, Derecho], [ValorActual, Izquierdo, NuevoDerecho]) :-
  Valor >= ValorActual,
  anadir_nodo(Valor, Derecho, NuevoDerecho).

/* ------------------------------------------------------------------------------------------------- */

% Predicado para eliminar un nodo del árbol
eliminar_nodo(Valor, [], []) :- !.

eliminar_nodo(Valor, [Valor, [], []], []) :- !.

eliminar_nodo(Valor, [Valor, Izquierdo, []], Izquierdo) :- !.

eliminar_nodo(Valor, [Valor, [], Derecho], Derecho) :- !.

eliminar_nodo(Valor, [ValorActual, Izquierdo, Derecho], NuevoArbol) :-
  Valor < ValorActual,
  eliminar_nodo(Valor, Izquierdo, NuevoIzquierdo),
  NuevoArbol = [ValorActual, NuevoIzquierdo, Derecho].

eliminar_nodo(Valor, [ValorActual, Izquierdo, Derecho], NuevoArbol) :-
  Valor > ValorActual,
  eliminar_nodo(Valor, Derecho, NuevoDerecho),
  NuevoArbol = [ValorActual, Izquierdo, NuevoDerecho].

eliminar_nodo(Valor, [ValorActual, Izquierdo, Derecho], NuevoArbol) :-
  Valor =\= ValorActual,
  eliminar_nodo(Valor, Izquierdo, NuevoIzquierdo),
  NuevoArbol = [ValorActual, NuevoIzquierdo, Derecho].

/* ------------------------------------------------------------------------------------------------- */

/* Para balancear un arbol en prolog primero creamos un predicado que tenga 2 argumentos: el árbol no balanceado de primero y el resultante de segundo, después transformaremos el arbol a una lista ordenada de valores para posteiormente rearmarla en función de un árbol */

% Predicado principal para equilibrar o balancear un arbol binario
equilibrar_arbol(ArbolDesequilibrado, ArbolEquilibrado) :-
    lista_arbol(ArbolDesequilibrado, Lista),
    construir_arbol(Lista, ArbolEquilibrado).

% Predicado para convertir un árbol a una lista ordenada

% Caso base con una lista vacia de elementos
lista_arbol([], []).

/* Recursión para la conversión de un arbol binario en una lista ordenada -> de forma recursiva el arbol se partira en sus subramas izquierda y derecha, para despues hacer 'merge' o juntar dichas listas en izquierda, derecha y raiz, las cuales al unirse daran la lista ordenada */
lista_arbol([Valor, Izquierdo, Derecho], Lista) :-
    lista_arbol(Izquierdo, ListaIzq),
    lista_arbol(Derecho, ListaDer),
    merge_sorted(ListaIzq, [Valor | ListaDer], Lista).

% Predicado para fusionar dos listas ordenadas en una sola lista ordenada
merge_sorted([], Lista, Lista).
merge_sorted(Lista, [], Lista).
merge_sorted([X | RestoX], [Y | RestoY], [X | MergedResto]) :-
    X =< Y,
    merge_sorted(RestoX, [Y | RestoY], MergedResto).
merge_sorted([X | RestoX], [Y | RestoY], [Y | MergedResto]) :-
    X > Y,
    merge_sorted([X | RestoX], RestoY, MergedResto).

% Predicado para construir un árbol equilibrado a partir de una lista ordenada
construir_arbol([], []).
construir_arbol([Valor], [Valor, [], []]).
construir_arbol(Lista, [ValorMedio, Izquierdo, Derecho]) :-
    length(Lista, N),
    N1 is N // 2,
    length(PrimeraMitad, N1),
    append(PrimeraMitad, [ValorMedio | SegundaMitad], Lista),
    construir_arbol(PrimeraMitad, Izquierdo),
    construir_arbol(SegundaMitad, Derecho).

/* ------------------------------------------------------------------------------------------------- */

/*
  Version original del codigo para imprimir el arbol

  imprimir_arbol([]) :- write('.'), nl. 

  imprimir_arbol([Valor, [], []]) :- write(Valor), nl. 

  imprimir_arbol([Valor, Izquierdo, Derecho]) :-
    imprimir_arbol(Izquierdo),
    write(Valor), nl,
    imprimir_arbol(Derecho).

  % Predicado para imprimir el árbol en un formato legible
  imprimir_arbol(Arbol) :-
      write('Árbol binario:'), nl,
      imprimir_arbol(Arbol).
*/

/* Versión mejorada por copilot para que sea más visual */

imprimir_arbol([], _) :- write('.'), nl. 

imprimir_arbol([Valor, [], []], Nivel) :-
  imprimir_nivel(Nivel),
  write(Valor), nl. 

imprimir_arbol([Valor, Izquierdo, Derecho], Nivel) :-
  Nivel1 is Nivel + 1,
  imprimir_arbol(Derecho, Nivel1),
  imprimir_nivel(Nivel),
  write(Valor), nl,
  imprimir_arbol(Izquierdo, Nivel1).

% Predicado para imprimir el árbol en un formato legible
imprimir_arbol(Arbol) :-
  nl,
  imprimir_arbol(Arbol, 0).

% Predicado auxiliar para imprimir espacios en blanco según el nivel del nodo
imprimir_nivel(0).
imprimir_nivel(Nivel) :-
  Nivel > 0,
  write('   '),
  Nivel1 is Nivel - 1,
  imprimir_nivel(Nivel1).
  
/* ------------------------------------------------------------------------------------------------- */

ad([10,
    [5,
        [2, [1, [], []], [3, [], []]],
        [7, [6, [], []], [8, [], []]]
    ],
    [15,
        [12, [11, [], []], [13, [], []]],
        [20,
            [17, [16, [], []], [18, [], []]],
            [25, [22, [], []], [30, [], []]]
        ]
    ]
]).

ejemplo_equilibrar :-
  ad(ArbolDesequilibrado),
  equilibrar_arbol(ArbolDesequilibrado, ArbolEquilibrado),
  write('Árbol desequilibrado:'), nl,
  imprimir_arbol(ArbolDesequilibrado),
  write('Árbol equilibrado:'), nl,
  imprimir_arbol(ArbolEquilibrado).

:- ejemplo_equilibrar.