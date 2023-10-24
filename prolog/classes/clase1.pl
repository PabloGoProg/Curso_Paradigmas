/* Definimos hechos, en ellos tenemos una relacion, la cual sera equiaente a una funcion, y un obejto, que es quien será evaluado */
es_roja(rosa).
es_verde(hoja).
es_azul(cielo).

/* Otro rpo de hecho es una relación, aquí podemos hacer validaciones entre varios objetos */
mas_grande(elefante, perro).
mas_grande(caballo, perro).
mas_grande(perro, raton).
mas_grande(raton, mosca).

/* Definimos reglas, en ellas tenemos una relacion, la cual sera equiaente a una funcion, y un obejto, que es quien será evaluado */
muy_grande(A, C) :- mas_grande(A, B), mas_grande(B, C).

/*
  Actividad árbol genealogico
*/

hijode(A,B) :- padrede(B,A).
abuelode(A,C) :- padrede(A,B), padrede(B,C).
hermanode(A,B) :- padrede(C,A), padrede(C,B), A \== B.
familiarde(A,B) :- padrede(A,B); abuelode(A,B); hermanode(A,B).