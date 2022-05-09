% Alumna: Ana Valeria Deloya Andrade


% margarita es mi abuela materna
madre(margarita,angelica). 
madre(margarita,socorro).
madre(margarita,veronica).
madre(margarita,lorena).

% francisco es mi abuelo materno
padre(francisco,angelica).
padre(francisco,socorro).
padre(francisco,veronica).
padre(francisco,lorena).

% angelica es mi mama
madre(angelica,valeria).
madre(angelica,jessica).

% rogelio es mi papa
padre(rogelio,valeria).
padre(rogelio,jessica).

hermanos(angelica,socorro).
hermanos(socorro,veronica).
hermanos(veronica,lorena).

% mi hermana
hermanos(valeria,jessica).

% rosalba es mi abuela paterna
madre(rosalba,rogelio). 
madre(rosalba,eduardo).

% francisco es mi abuelo paterno
padre(francisco,rogelio).
padre(francisco,eduardo).

hermanos(rogelio,eduardo).

hermanos(X,Y) :- hermanos(Y,X).






















