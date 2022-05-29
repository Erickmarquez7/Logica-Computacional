
%definicion de trib:
%trib 0 = 0
%trib 1 = 0
%trib 2 = 0
%trib 3 = 1
%trib n = trib(n-1)+trib(n-2)+ trib(n-3) 

trib(0,0).
trib(1,0).
trib(2,0).
trib(3,1).
trib(N,X) :- N > 3, N1 is N-1, trib(N1,X1), N2 is N-2, trib(N2,X2), N3 is N-3, trib(N3,X3), X is X1+X2+X3.	


%definicion de factorial
%factorial 0 = 1
%factorial x = x fact (x-1)
fact(0,1).
fact(X,Y) :- X>0, A is X-1, fact(A,B), Y is X*B.

%Dado un numero calcula la suma de todos los naturales hasta dicho número
%cuenta 0 = 0
%cuenta 1 = 1
%cuenta n = n + cuenta(n-1) 
cuenta(0,0).
cuenta(1,1).
cuenta(N,X) :- N>1, N1 is N-1 , cuenta(N1,X1), X is N+X1.