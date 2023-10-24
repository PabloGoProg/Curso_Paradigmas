/* 
  Método usado para crearun árbol vacio 
  nil - es equilanete a [], es usado para crear listas o estructuras de datoos vacias
*/
arbol(nil).

/*
  Este el caso base para añadir un nodo, aquí verificamos si el arbol dado por parametros es un árbol vacio, para después añadirse cómo una raiz.
*/
anadir_nodo(Valor, [], [Valor, [], []]).

anadir_nodo(Valor, [ValorActual, Izquierdo, Derecho], [ValorActual, NuevoIzquierdo, Derecho]) :-
  Valor < ValorActual,
  añadir_nodo(Valor, Izquierdo, NuevoIzquierdo).

anadir_nodo(Valor, [ValorActual, Izquierdo, Derecho], [ValorActual, Izquierdo, NuevoDerecho]) :-
  Valor >= ValorActual,
  añadir_nodo(Valor, Izquierdo, NuevoDerecho).

% Predicado para eliminar un nodo del árbol
eliminar_nodo(Valor, Arbol, NuevoArbol) :-
    % Implementa la lógica para eliminar un nodo del árbol.
    % Esto puede requerir diferentes casos para manejar nodos hoja,
    % nodos con un solo hijo o nodos con dos hijos.

% Predicado para editar el valor de un nodo
editar_nodo(ValorViejo, ValorNuevo, Arbol, NuevoArbol) :-
    % Implementa la lógica para buscar el nodo con ValorViejo
    % y reemplazar su valor por ValorNuevo.

% Predicado para equilibrar el árbol
equilibrar_arbol(ArbolDesbalanceado, ArbolEquilibrado) :-
    % Implementa la lógica para equilibrar el árbol desbalanceado.
    % Puedes utilizar rotaciones u otras estrategias para equilibrarlo.

% Predicado para buscar un nodo en el árbol
buscar_nodo(Valor, Arbol) :-
    % Implementa la lógica para buscar un nodo en el árbol.
    % Puedes utilizar recursión para recorrer el árbol.

% Predicado para imprimir el árbol
imprimir_arbol(Arbol) :-
    % Implementa la lógica para imprimir el árbol en un formato legible.

% Ejemplos de consultas y uso del programa
ejemplo :-
    arbol(ArbolVacio),
    anadir_nodo(10, ArbolVacio),
    anadir_nodo(5, Arbol1, Arbol2),
    anadir_nodo(15, Arbol2, Arbol3),
    eliminar_nodo(5, Arbol3, Arbol4),
    editar_nodo(10, 12, Arbol4, Arbol5),
    equilibrar_arbol(Arbol5, ArbolEquilibrado),
    buscar_nodo(12, ArbolEquilibrado),
    imprimir_arbol(ArbolEquilibrado).
