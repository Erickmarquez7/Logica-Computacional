
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
