% Alumna: Natalia Abigail Pérez Romero


% Maria es mi abuela materna
madre(maria,iris). 
madre(maria,ana).
madre(maria,alba).
abuelo(natalia,maria).

% Son hermanas
hermanos(iris,ana).
hermanos(iris,alba).
hermanos(alba,ana).

% Sergio es mi abuelo materno
padre(sergio,iris).
padre(sergio,ana).
padre(sergio,alba).
abuelo(natalia,sergio).

% Iris es mi mama
madre(iris,natalia).

% Angel es mi papa
padre(angel,natalia).

% Carmen es mi abuela paterna
madre(carmen,angel).
abuelo(natalia,carmen). 

% Alfonso es mi abuelo paterno
padre(alfonso,angel).

% Abuelo
abuelo(natalia,alfonso).

hermanos(X,Y) :- hermanos(Y,X).

                                
-        Carmen   Alfonso        Maria    Sergio              
-            \    /             /       |     \
-             Angel          Iris  -  Alba - Ana
-                \          /
-                yo (natalia)
