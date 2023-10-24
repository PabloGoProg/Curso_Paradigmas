/* Declaración de los hechos solicitados */

/*
  Declaramos a los hombres del árbol familiar.
*/
hombre(matias).
hombre(luis).
hombre(martin).
hombre(rafael).
hombre(jose).
hombre(jaime).

/*
  Declaramos a las mujeres del árbol familiar.
*/
mujer(salome).
mujer(sara).
mujer(daniela).
mujer(rosario).
mujer(lina).
mujer(marta).

/*
  Declaramos las relaciones hijo - padre|madre del árbol familiar superior.
*/
es_hijo_de(rosario, jose).
es_hijo_de(rosario, marta).
es_hijo_de(rafael, jose).
es_hijo_de(rafael, marta).

/*
  Declaramos las relaciones hijo - padre|madre del árbol familiar inferior izquierdo.
*/
es_hijo_de(salome, luis).
es_hijo_de(salome, rosario).
es_hijo_de(matias, luis).
es_hijo_de(matias, rosario).
es_hijo_de(sara, luis).
es_hijo_de(sara, rosario).

/*
  Declaramos las relaciones hijo - padre|madre del árbol familiar inferior derecho.
*/
es_hijo_de(martin, rafael).
es_hijo_de(martin, lina).
es_hijo_de(daniela, rafael).
es_hijo_de(daniela, lina).

/*
  Declaramos las relaciones de conyugues del árbol familiar.
*/
es_conyugue_de(jose, marta).
es_conyugue_de(marta,jose).
es_conyugue_de(luis, rosario).
es_conyugue_de(rosario,luis).
es_conyugue_de(rafael, lina).
es_conyugue_de(lina, rafael).

/* 1.
  validamos si el padre de B es hijo de A para validar si es abuelo de B, lo mismo para las madres, ya por último validamos el género.
*/
es_abuelo_de(A,C):-es_hijo_de(C,B),es_hijo_de(B,A),hombre(A).
es_abuela_de(A,C):-es_hijo_de(C,B),es_hijo_de(B,A),mujer(A).

/* 2.
  Tenemos la rela de que A será el padre de B, lo mismo que decir que B será el hijo de A, por último comprobamos el género.
*/
es_padre_de(A,B):-es_hijo_de(B,A),hombre(A).
es_madre_de(A,B):-es_hijo_de(B,A),mujer(A).

/* 3.
  Validamos que tanto A cómo B sean hijos del mismo padre o madre, para posteriormente comprobar el genero.
*/
es_hermana_de(A,B):-es_hijo_de(A,C),es_hijo_de(B,C),mujer(A),A\==B.
es_hermano_de(A,B):-es_hijo_de(A,C),es_hijo_de(B,C),hombre(A),A\==B.
are_sibblings_of(A,B):-es_hermana_de(A,B);es_hermano_de(A,B).

/* Requerimos de crear un hecho para retratar la relación de hermanos entre Jiame y Marta */

es_hermana_de(marta, jaime).
es_hermano_de(jaime, marta).

/* 4.
  Suponemos cómo regla que A será el tio de B, para la solución comprobamos que A sea hermano de un C, que este C tenga un hijo B.
*/
es_tio_de(A,B):-es_hermano_de(A,C),es_hijo_de(B,C),not(es_hijo_de(B,A)).
es_tia_de(A,B):-es_hermana_de(A,C),es_hijo_de(B,C),not(es_hijo_de(B,A)).

/* 5.
  Para comprobar si un A es primo de un B, es necesario verificar que tanto A como B sean hijos de padres distintos y que estos padres (C y D) sean hermanos.
*/
es_primo_de(A,B):-es_hijo_de(A,C),es_hijo_de(B,D),are_sibblings_of(C,D),C\=D.

/* 6.
  Para validar si un A es cuñado de un B es necesario validarlo de dos formas opuestas la una a la otra:
  1. - Validamos si A es conyugue de C para despues validar si C es herman@ de B.
  2. - validamos si A es herman@ de C para después verificar si C es conyugue de B.
*/
es_cunado_de(A,B):-es_conyugue_de(A,C),are_sibblings_of(C,B);are_sibblings_of(A,C),es_conyugue_de(C,B).

/* 7. 
  Validar un A sea un sobrino de B es equivalente a preguntar que un B sea ti@ de un A.
*/
es_sobrino_de(A,B):-es_tio_de(B,A);es_tia_de(B,A).

/* 8.
  Para validar que un A sea suegr@ de un B, primero validamos que B sea conyugue de un C y que ese A sea su padre o madre dependiendo el género buscado.
*/
es_suegro_de(A,B):-es_conyugue_de(B,C),es_padre_de(A,C).
es_suegra_de(A,B):-es_conyugue_de(B,C),es_madre_de(A,C).
