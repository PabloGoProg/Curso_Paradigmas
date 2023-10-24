es_hombre(juan).
es_hombre(pedro).
es_hombre(jose).

hombres([]).

/* Aquí se ejecuta una recursión, por cada elemento x, se evalue si este es hombre y después se hace el llamado recursivo llamdando a hombres con el valor Y, quien es el proximo en la cabeza de la lista. */
hombres([X|Y]) :- es_hombre(X), hombres(Y).

no_pertenece(x,[]).
no_pertenece(X,[Y|Z]) :- X \= Y, no_pertenece(X,Z).