% Aquí va el código.

%1era entrega:

%PUNTO 1:
%ESTO NO HARIA FALTA
%jugador(ana).
%jugador(beto).
%jugador(carola).
%jugador(dimitri).

civilizacion(incas).
civilizacion(romanos).

juega(ana,romanos).
juega(beto,incas).
juega(carola,romanos).
juega(dimitri,romanos).

tecnologia(herreria).
tecnologia(forja).
tecnologia(emplumado).
tecnologia(laminas).
tecnologia(fundicion).

tecnologiasDesarrolladas(ana,herreria).
tecnologiasDesarrolladas(ana,forja).
tecnologiasDesarrolladas(ana,emplumado).
tecnologiasDesarrolladas(ana,laminas).

tecnologiasDesarrolladas(beto,forja).
tecnologiasDesarrolladas(beto,fundicion).
tecnologiasDesarrolladas(beto,herreria).

tecnologiasDesarrolladas(carola,herreria).

tecnologiasDesarrolladas(dimitri,fundicion).
tecnologiasDesarrolladas(dimitri,herreria).

%PUNTO 2:
esExpertoEnMetales(Jugador):-
    %jugador(Jugador),
    tecnologiasDesarrolladas(Jugador,herreria),
    tecnologiasDesarrolladas(Jugador,forja),
    juega(Jugador,romanos).

esExpertoEnMetales(Jugador):-
    %jugador(Jugador),
    tecnologiasDesarrolladas(Jugador,herreria),
    tecnologiasDesarrolladas(Jugador,forja),
    tecnologiasDesarrolladas(Jugador,fundicion).

/*
desarrolló herrería && desarrolló forja && (desarrolló fundición || juega con los romanos)
*/

%PUNTO 3:
civilizacionPopular(Civilizacion):-
    %civilizacion(Civilizacion),
    juega(Jugador1,Civilizacion),
    juega(Jugador2,Civilizacion),
    Jugador1 \= Jugador2.

%PUNTO 4:
tieneAlcanceGlobal(Tecnologia):-
    %tecnologiasDesarrolladas(Jugador,Tecnologia),
    tecnologia(Tecnologia),
    forall(juega(Jugador,_),tecnologiasDesarrolladas(Jugador,Tecnologia)).  % para todos los jugadores si desarrollaron esa tecnologia (tecnologia ligada)

%forall(universoAcomprobar,dondeSeCompruebaCadaElementoDelUniverso).

%PUNTO 5:
civilizacionEsLider(Civilizacion):-
    civilizacion(Civilizacion),
    forall(civilizacionAlcanzoUnaTecnologia(_,Tecnologia),civilizacionAlcanzoUnaTecnologia(Civilizacion,Tecnologia)).    
%primer argumento: las tecnologias que tienen asociada una civilizacion

civilizacionAlcanzoUnaTecnologia(Civilizacion,Tecnologia):-
    juega(Jugador,Civilizacion), % tiene que ser un jugador
    tecnologiasDesarrolladas(_,Tecnologia).

%PRIMERA ENTREGA TERMINADA

%2da entrega:

campeon(Vida) :-
    integer(Vida), %integer/1 se utiliza para verificar que un valor es un número entero.
    Vida >= 1,
    Vida =< 100.
    
jinete(caballo).
jinete(camello).

piquero(Nivel, Escudo) :-
    integer(Nivel), %integer/1 se utiliza para verificar que un valor es un número entero.
    Nivel >= 1,
    Nivel =< 3,
    member(Escudo,[conEscudo,sinEscudo]). % member/2 se utilizó para verificar si un elemento pertenece a una lista. Esto garantiza que los valores asignados a ciertos atributos de las unidades sean válidos y pertenecen a un conjunto predefinido de opciones.

%PUNTO 6

unidadAna(ana,[caballo,piquero(1,conEscudo),piquero(2,sinEscudo)]).
unidadBeto(beto,[caballo,piquero(1,conEscudo),campeon(100),campeon(80)]).
unidadCarolo(carola,[piquero(3,sinEscudo),piquero(2,conEscudo)]).
unidadDimitri(dimitri,[]).

% USE listas, utilizando corchetes [] para agrupar múltiples elementos ---> (jugador,[las unidades del jugador])}

%PUNTO 7

vidaJineteCaballo(90).
vidaJineteC(90).

unidadConMasVida(Jugador) :-
    juega(Jugador,_).
    





