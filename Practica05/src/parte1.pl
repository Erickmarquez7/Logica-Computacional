
%definicion "Informal" de trib:
%trib 0 = 0
%trib 1 = 0
%trib 2 = 0
%trib 3 = 1
%trib n = trib(n-1)+trib(n-2)+ trib(n-3) 

trib(0,0).
trib(1,0).
trib(2,0).
trib(3,1).
trib(N,X) :- N > 1, trib(N-3,X1),trib(n-2,X2),trib(n-1,X3), X is X1+X2+X3.	


%defincion de fact
%fact 0 = 1
%fact 1 = 1
%fact 2 = 2
%fact 3 = 6
%fact 4 = 24
%fact 5 = 120
%asi

fact(0,0).
fact(1,0).
fact(N,X) :- N>1, fact(N-1, XS), X is N*XS.

%Lo de suma
%suma 0 = 0
%suma 1 = 1
%suma 2 = 3
%suma 3 = 6
%suma 4 = 10
%suma 5 = 15
suma(0,0).
suma(1,1).
suma(N,X) :- N>1, suma(N-1, XS), X is N+XS.


% Ps lo binarios xd
% Or
or(0,0,0).
or(1,0,1).
or(0,1,1).
or(1,1,1).

% And
and(0,0,0).
and(1,0,0).
and(0,1,0).
amd(1,1,1).

%Negacion
not(1,0).
not(0,1).

