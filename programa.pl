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

tecnologia(Tecnologia) :- tecnologiasDesarrolladas(_, Tecnologia).

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

juegaV2(ana, romanos, tecnologias([herreria,forja,emplumado,laminas])).
juegaV2(beto, incas, tecnologias([forja,fundicion,herreria])).
juegaV2(carola, romanos, tecnologias([herreria])).
juegaV2(dimitri, romanos, tecnologias([fundicion,herreria])).

%PUNTO 2:
esExpertoEnMetales(Jugador):-
    %jugador(Jugador),
    tecnologiasDesarrolladas(Jugador,herreria),
    tecnologiasDesarrolladas(Jugador,forja),
    juegaRomanosODesarrollaFundicion(Jugador).

juegaRomanosODesarrollaFundicion(Jugador) :-
    tecnologiasDesarrolladas(Jugador,herreria);
    juega(Jugador,romanos).

%juegaRomanosODesarrollaFundicion(Jugador) :-
    

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
    tecnologiasDesarrolladas(_, Tecnologia),
    forall(juega(Jugador,_),tecnologiasDesarrolladas(Jugador,Tecnologia)).  % para todos los jugadores si desarrollaron esa tecnologia (tecnologia ligada)

%forall(universoAcomprobar,dondeSeCompruebaCadaElementoDelUniverso).

%PUNTO 5:
civilizacionEsLider(Civilizacion):-
    juega(_, Civilizacion),
    forall(civilizacionAlcanzoUnaTecnologia(_,Tecnologia),civilizacionAlcanzoUnaTecnologia(Civilizacion,Tecnologia)).    
%primer argumento: las tecnologias que tienen asociada una civilizacion

civilizacionAlcanzoUnaTecnologia(Civilizacion,Tecnologia):-
    %civilizacion(Civilizacion),
    juega(Jugador,Civilizacion), % tiene que ser un jugador
    tecnologiasDesarrolladas(Jugador,Tecnologia).
% ta raro el caso de incas!! (REVISAR) --> REVISADO

%PRIMERA ENTREGA TERMINADA

%2da entrega:

%campeon(Vida) :-
%    integer(Vida), %integer/1 se utiliza para verificar que un valor es un número entero.
    %Vida >= 1,
    %Vida =< 100.
%    between(1, 100, Vida). %"seria como un entre"
    
%jinete(caballo).
%jinete(camello).

%piquero(Nivel, Escudo) :-
%    integer(Nivel), %integer/1 se utiliza para verificar que un valor es un número entero.
    %Nivel >= 1,
    %Nivel =< 3,
%    between(1, 3, Nivel),
%    member(Escudo,[conEscudo,sinEscudo]). % member/2 se utilizó para verificar si un elemento pertenece a una lista. Esto garantiza que los valores asignados a ciertos atributos de las unidades sean válidos y pertenecen a un conjunto predefinido de opciones.

%PUNTO 6

%unidadesQueTiene(Jugador,[unidades]).
unidadesQueTiene(ana, [jinete(caballo),piquero(1,conEscudo),piquero(2,sinEscudo)]).
unidadesQueTiene(beto, [jinete(caballo),piquero(1,conEscudo),campeon(100),campeon(80)]).
unidadesQueTiene(carola, [piquero(3,sinEscudo),piquero(2,conEscudo)]).
unidadesQueTiene(dimitri, []).

unidadQueTiene(ana, jinete(caballo)).
unidadQueTiene(ana, piquero(1, conEscudo)).
unidadQueTiene(ana, piquero(2, sinEscudo)).

unidadQueTiene(beto, jinete(caballo)).
unidadQueTiene(beto, piquero(1, conEscudo)).
unidadQueTiene(beto, campeon(100)).
unidadQueTiene(beto, campeon(80)).

unidadQueTiene(carola, piquero(3, sinEscudo)).
unidadQueTiene(carola, piquero(2, conEscudo)).

% USE listas, utilizando corchetes [] para agrupar múltiples elementos ---> (jugador, unidades([las unidades del jugador])).

%PUNTO 7

%vidaJineteCaballo(90).
%vidaJineteCamello(80).

%vidaUnidad(Unidad, Vida).
vidaUnidad(jinete(caballo), 90).
vidaUnidad(jinete(camello), 80).
vidaUnidad(campeon(Vida), Vida).
vidaUnidad(piquero(1,sinEscudo), 50).
vidaUnidad(piquero(2,sinEscudo), 65).
vidaUnidad(piquero(3,sinEscudo), 70).

vidaUnidad(piquero(Nivel,conEscudo), Vida) :-
    vidaUnidad(piquero(Nivel,sinEscudo),VidaSinEscudo),
    Vida is VidaSinEscudo * 1.1. % le sumo el 10%

unidadConMasVida(Jugador, UnidadConMasVida) :-
    unidadQueTiene(Jugador, UnidadConMasVida),
    vidaUnidad(UnidadConMasVida, VidaMayor),
    forall((unidadQueTiene(Jugador, OtraUnidad), vidaUnidad(OtraUnidad, VidaMenor), OtraUnidad \= UnidadConMasVida), VidaMayor > VidaMenor).

tieneMayorVida(Unidad1, Unidad2) :-
    vidaUnidad(Unidad1, Vida1),
    vidaUnidad(Unidad2, Vida2),
    Vida1 > Vida2.

% PUNTO 8:
% Si una unidad le gana a otra

leGana(UnaUnidad, OtraUnidad) :-
    vidaUnidad(UnaUnidad,_),        % ambas son unidades
    vidaUnidad(OtraUnidad, _),
    tieneVentajaSobre(UnaUnidad, OtraUnidad).
    
leGana(UnaUnidad, OtraUnidad) :-
    vidaUnidad(UnaUnidad,_),        % ambas son unidades
    vidaUnidad(OtraUnidad, _),
    not(tieneVentajaSobre(OtraUnidad,UnaUnidad)),
    tieneMayorVida(UnaUnidad, OtraUnidad).

tieneVentajaSobre(jinete(_), campeon(_)).   % Cualquier jinete le gana a cualquier campeón
tieneVentajaSobre(campeon(_), piquero(_,_)). % Cualquier campeón le gana a cualquier piquero
tieneVentajaSobre(piquero(_,_), jinete(_)). % Cualquier piquero le gana a cualquier jinete
tieneVentajaSobre(jinete(camello), jinete(caballo)). % Los jinetes a camello le ganan a los de caballo

%PUNTO 9
%Si un jugador puede sobrevivir a un asedio
sobreviveAsidio(Jugador) :-
    unidadQueTiene(Jugador,_),
    cantidadUnidades(Jugador, piquero(_,conEscudo) , CantConEscudo),
    cantidadUnidades(Jugador, piquero(_,sinEscudo) , CantSinEscudo),
    CantConEscudo > CantSinEscudo.

cantidadUnidades(Jugador, Unidad, CantidadUnidad) :-
    %unidadQueTiene(Jugador,Unidad),
    findall(Unidad, unidadQueTiene(Jugador,Unidad), ListaUnidades),
    length(ListaUnidades,CantidadUnidad).

%PUNTO 10 ()

dependencia(herreria, emplumado).
dependencia(herreria, forja).
dependencia(herreria, laminas).
dependencia(emplumado, punzon).
dependencia(forja, fundicion).
dependencia(fundicion,horno).
dependencia(laminas,malla).
dependencia(malla, placas).
dependencia(molino, collera).
dependencia(collera, arado).

/*
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
*/

esTecnologia(Tecnologia) :- dependencia(Tecnologia,_).
esTecnologia(Tecnologia) :- dependencia(_,Tecnologia).

puedeDesarrollarTecnologia(Jugador, Tecnologia):-
    juega(Jugador,_),
    esTecnologia(Tecnologia),
    not(tecnologiasDesarrolladas(Jugador, Tecnologia)),
    forall(dependencia(TecnologiaRequerida, Tecnologia), tecnologiasDesarrolladas(Jugador, TecnologiaRequerida)).



