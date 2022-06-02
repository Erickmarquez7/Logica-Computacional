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
bt(node(A, T1, T2)) :- integer(A), bt(T1), bt(T2).

/*1. Comprueba si elemento del arbol*/
elem(A, bt(node(E, T1, T2))) :- A is E; %E el elemento a buscar lo comparamos con A, el padre
                                elem(A, T1); %si no, en los hijos
                                elem(A, T2). 
/*Al parecer es diferente , que ; la , es com un and, el ; es como un or
 */



/*2. Comprueba que A es menor a todos los elementos de T*/

minelem(A, bt(node(E, T1, T2))) :- A < E;
                                minelem(A,T1),
                                minelem(A,T2).

/* ejemplos de arboles
elem(2,bt(node(3,bt(void),bt(void))))
elem(2,bt(node(3,bt(node(3,bt(void),bt(void))),bt(node(3,bt(void),bt(node(2,bt(void),bt(void))))))))

minelem(2,bt(node(4,bt(node(3,bt(void),bt(void))),bt(node(8,bt(void),bt(node(5,bt(void),bt(void))))))))
*/