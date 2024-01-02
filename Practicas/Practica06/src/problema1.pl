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

/* Problema 1:
Una casa tiene seis cuartos y siete puertas, en el cuarto g se
encuentra un teléfono.
1. Escribe un programa en prolog que represente la información 
antes dada.
*/
/*¿Es posible pasar de un cuarto a otro?*/
puerta(e,f).
puerta(e,d).
puerta(e,g).
puerta(e,b).
puerta(d,c).
puerta(b,a).
puerta(b,c).
puerta(X,Y) :- puerta(Y,Z), X = Z, !.

/* ¿El cuarto tiene un telefono? */
telefono(g).

/*Cuando estamos en un cuarto*/
state(a,b,c,d,e,f,b).
/* Definir cuando nos cambiamos de cuarto 
¿Qué movimientos tenemos?
Entrar a cuarto X desde cuarto Y*/
cambio(pasar_a_b, puerta(a,b), state(b)).
cambio(pasar_b_a, puerta(b,a), state(a)).

cambio(pasar_b_c, puerta(b,c), state(c)).
cambio(pasar_c_b, puerta(c,b), state(b)).

cambio(pasar_b_e, puerta(b,e), state(e)).
cambio(pasar_e_b, puerta(e,b), state(b)).

cambio(pasar_d_e, puerta(d,e), state(e)).
cambio(pasar_e_d, puerta(e,d), state(d)).

cambio(pasar_e_f, puerta(e,f), state(f)).
cambio(pasar_f_e, puerta(e,f), state(e)).

cambio(pasar_e_g, puerta(e,g), state(g)).
cambio(pasar_g_e, puerta(g,e), state(e)).

cambio(pasar_d_c, puerta(d,c), state(c)).
cambio(pasar_c_d, puerta(c,d), state(d)).

/*
2. Sobre el programa, da las reglas para encontrar un camino que 
nos lleve desde a hasta el teléfono
*/
path(Init,End,Visited, [Cambio | Path]) :-
    cambio(Cambio, Init, Qn),
    \+ member(Qn, Visited),
    path(Qn,End,[Qn|Visited], Path).


/*
3. Modifica el programa para no pasar por lo cuartos f ni d
*/
path3(Init,End,Visited, [Cambio | Path3]) :-
    cambio(Cambio, Init, Qn),
    \+ member(Qn, Visited),,
    not (member(puerta(e,f)),Path3), %puerta f
    not (member(puerta(e,d)),Path3), %puerta d 
    not (member(puerta(d,c)),Path3), %puerta d
    path3(Qn,End,[Qn|Visited], Path3).
%solo ponemos que no sean miembros f ni d

%%%%%%f