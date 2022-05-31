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

/*1. verifica si un objeto se encuentra en la lista*/
%[1,2,4], 4
%[5,2,3], 4, nop
%
esta([X|_],X). %caso base, c concentra en la cabeza
esta([_|XS],X):- esta(XS,X). %C concentra en la cola


/*2. Borra un elemento de la lista*/
% 3, [1,2,3,4,5], = [1,2,4,5] true
% 4, [5,6,7,8], = [5,6,7,8] false
% 3, [1,5,9,0,3], = [1,5,9,0] true
borrar(X, [X|XS], XS). %eliminamos, nos quedamos con la cola
borrar(X, [L|LS], [M|MS]) :- not(X is M), borrar(X,LS,MS).