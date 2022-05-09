hermano(X,Y) :- hermano(Y,X).
casados(X,Y) :- casados(Y,X).

%%mis abuelos maternos
casados(emilia,ruben).

%% madre de mi madre, es decir mi abue, que a su vez son hijos de los hermanos de mi madre
madre(emilia,adri).
madre(emilia,vale).
madre(emilia,pilar).
madre(emilia,rosa).
madre(emilia,alfonso).
madre(emilia,jorge).
madre(emilia,carlos).
madre(emilia,raul).

%%padre de mi madre
padre(ruben,adri).
padre(ruben,vale).
padre(ruben,pilar).
padre(ruben,rosa).
padre(ruben,alfonso).
padre(ruben,jorge).
padre(ruben,carlos).
padre(ruben,raul).


%%mis abuelos paterno
casados(ofelia,guille).

%%la madre de mi padre
madre(ofelia, mario).
madre(ofelia, isidro).
madre(ofelia, elena).
madre(ofelia, lupe).

%%el padre de mi padre
padre(guille,mario).
padre(guille,isidro).
padre(guille,elena).
padre(guille,lupe).


%%los hermanos de mi madre
hermano(adri,vale).
hermano(adri,pilar).
hermano(adri,rosa).
hermano(adri,alfonso).
hermano(adri,jorge).
hermano(adri,carlos).
hermano(adri,raul).

%%los hermanos de mi padre
hermano(mario,isidro).
hermano(mario,elena).
hermano(mario,lupe).

%%mis padres
casados(adri,mario). 

%% mi madre y padre con mis hermanos
madre(adri, erick).
padre(mario, erick).

madre(adri,jacinto).
padre(mario, jacinto).

madre(adri, dante).
padre(mario, dante).

hermano(erick,jacinto).
hermano(erick,dante).



%           ofelia - guille                                          emilia - ruben           %
%     |------------|---------|-----------|      |--------|---------|--------|-----------|----------|-----------|---------|                                                       %
%   isidro       elena      lupe       mario - adri     vale     pilar     rosa      alfonso     jorge      carlos      raul      %
%                                  |---------|---------|       %
%                                 erick     jacinto   dante     %

