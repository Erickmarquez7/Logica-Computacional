/*
- Lógica Computacional 2022-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: José Ricardo Desales Santos
- Laboratorio: Emiliano Galeana Araujo
- Integrantes:
- Bernal Márquez Erick        317042522
- Deloya Andrade Ana Valeria  317277582
- Perez Romero Natalia Abigail 318144265
*/
padre(protagonista,padre).
padre(X,Y) :- padre(Y,Z), X = Z, !.

esposos(protagonista,esposa).
esposos(padre,hija1).
esposos(X,Y) :- esposos(Y,Z), X = Z, !.

suegro(X,Y) :- padre(Y,Z), Z is esposos(X,_).

abuelo(X,X).
