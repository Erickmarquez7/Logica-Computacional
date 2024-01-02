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

/*definicion de trib:
trib 0 = 0
trib 1 = 0
trib 2 = 0
trib 3 = 1
trib n = trib(n-1)+trib(n-2)+ trib(n-3)*/

trib(0,0).
trib(1,0).
trib(2,0).
trib(3,1).
trib(N,X) :- N > 3, N1 is N-1, trib(N1,X1), N2 is N-2, trib(N2,X2), N3 is N-3, trib(N3,X3), X is X1+X2+X3.	
%al parecer es diferente poner N1 que N-1, no c pk


%definicion de factorial
%factorial 0 = 1
%factorial x = x fact (x-1)
fact(0,1).
fact(1,1).
fact(N,X) :- N > 1, N1 is N-1, fact(N1,XS), X is N * XS.


%Dado un numero calcula la suma de todos los naturales hasta dicho número
%cuenta 0 = 0
%cuenta 1 = 1
%cuenta n = n + cuenta(n-1) 
cuenta(0,0).
cuenta(1,1).
cuenta(N,X) :- N>1, N1 is N-1 , cuenta(N1,X1), X is N+X1.	


/* 3. suma los primeros n numeros
%cuenta 0 = 0
%cuenta 1 = 1
%cuenta 2 = 3
%cuenta 3 = 6
%cuenta 4 = 10
%cuenta 5 = 15*/
cuenta(0,0).
cuenta(1,1).
cuenta(N,X) :- N>1, N1 is N-1, suma(N1, XS), X is N+XS.


/* 4. suma todos los elementos de una lista
%suma [1,2,3] = 6
%suma [2,5,9] = 16
%suma [0,5,2] = 7
%suma [3,2,9] = 14*/
suma([],0).
suma([X|XS],N) :- suma(XS, N1), N is N1+X.

/* 5. circuitos
% Ps lo binarios xd*/
% Or
or(false,false,false).
or(true,false,true).
or(false,true,true).
or(true,true,true).

% And
and(false,false,false).
and(true,false,false).
and(false,true,false).
and(true,true,true).

%Negacion
not(true,false).
not(false,true).


circuitoa(X,Y,R) :- not(X,P), %negamos a X como P 
                    and(P,Y,Q), %conjuncion de la negacion de X y Y como Q
                    not(Q,S), %negacion del resultado anterior
                    and(S,Y,R). %conjuncion de la negacion anterior y Y.

circuitob(X,Y,Z,R) :- not(X,P), %negamos a X como P
                      and(X,P,Q), %conjuncion de X y su negacion como Q
                      not(Q,S), %volvemos a negar xd
                      and(Y,Z,T), %conjuncion de Y y Z
                      and(S,T,R). %conjuncion de los dos resultados anteriores
