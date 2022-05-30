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
%defincion del arbol
bt(void).
bt(node(A, T1, T2)) :- interger(A), bt(T1), bt(T2).

/*1. Comprueba si elemento del arbol*/
elem(A, bt(node(E, T1, T2))) :- A=E; %E el elemento a buscar lo comparamos con A, el padre
                                elem(A, T1); %si no, en los hijos
                                elem(A, T1). 
/*Al parecer es diferente , que ; la , es com un and, el ; es como un or
 ejemplo xd bt = bt(2,node(void),node(void)).? xd
 */






