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

juegaV2(ana, romanos, tecnologias([herreria,forja,emplumado,laminas])).
juegaV2(beto, incas, tecnologias([forja,fundicion,herreria])).
juegaV2(carola, romanos, tecnologias([herreria])).
juegaV2(dimitri, romanos, tecnologias([fundicion,herreria])).

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

% Cada campeón tiene una vida distinta
% vidaUnidad(campeon(Vida),Vida) :-
%    campeon(Vida), % para que la vida que le ponga se encuentre en 1 y 100 (limites incluidos)
%    Vida = Vida.

% Los jinetes a camello tienen 80 de vida y los jinetes a caballo tienen 90.
% vidaUnidad(jinete(caballo),Vida) :-
%     Vida = 90.
% vidaUnidad(jinete(camello),Vida) :-
%     Vida = 80.

% Los piqueros sin escudo de nivel 1 tienen vida 50, los de nivel 2 tienen vida 65 y los de nivel 3 tienen 70 de vida.-
% vidaUnidad(piquero(1,sinEscudo),Vida) :-
%     Vida = 50.
% vidaUnidad(piquero(2,sinEscudo),Vida) :-
%     Vida = 65.
% vidaUnidad(piquero(3,sinEscudo),Vida) :-
%     Vida = 70.

% Los piqueros con escudo tienen 10% más de vida que los piqueros sin escudo.
% vidaUnidad(piquero(1,conEscudo),Vida) :-
%     Vida = 55.                   % 50 + (0.1 * 50).  % me lo va a mostrar asi tal cual en la terminal
% vidaUnidad(piquero(2,conEscudo),Vida) :-
%     Vida = 71.                   % 65 + (0.1 * 65).
% vidaUnidad(piquero(3,conEscudo),Vida) :-
%     Vida = 77.                   % 70 + (0.1 * 70).

%unidadConMasVidaa(Jugador, UnidadConMasVida) :-
%    juega(Jugador,_),                       % el jugador juega
%    unidadesQueTiene(Jugador,unidades(Unidadades)),     % 
%    findall(VidaUnidad, (member(Unidad,Unidadades), vidaUnidad(Unidad,VidaUnidad)), VidasUnidades),  
%    max_member(UnidadConMasVida, VidasUnidades).
    %maximo(VidasUnidades, UnidadConMasVida).

% OJO CAROLA ME DA 70 Y ME DEBERIA DAR 71.5

unidadConMasVida(Jugador, UnidadConMasVida) :-
    juega(Jugador, _),
    unidadesQueTiene(Jugador, Unidades),
    member(UnidadConMasVida, Unidades),
    findall(VidaUnidad, (member(Unidad, Unidades), vidaUnidad(Unidad, VidaUnidad)), VidasUnidades),
    max_member(UnidadMaxima, VidasUnidades),
    vidaUnidad(UnidadConMasVida,UnidadMaxima).

%unidadConMasVidaV2(Jugador, UnidadConMasVida) :-
%    juega(Jugador, _),
%    unidadesQueTiene(Jugador, Unidades),
%    member(UnidadConMasVida,Unidades),
%    forall(member(OtraUnidad,Unidades), (OtraUnidad \= UnidadConMasVida, tieneMayorVida(UnidadConMasVida, OtraUnidad))).
% HAY UN PROBLEMA CON ESTE PREDICADO unidadConMasVida, ME devuelve la Vida de la unidad con mas vida, pero no la unidad en si :(

tieneMayorVida(Unidad1, Unidad2) :-
    vidaUnidad(Unidad1, Vida1),
    vidaUnidad(Unidad2, Vida2),
    Vida1 > Vida2.

% listaDeVidas(Jugador, VidasUnidades) :-
%     juega(Jugador,_),
%     findall(unidadadesQueTiene(Jugador,Unidad), vidaUnidad(Unidad,Vida), VidasUnidades).
    
%maximo([X], X). %CASO BASE
%maximo([X|Xs], X) :-
%     maximo(Xs, Y),
%     X >= Y. % Si X es mayor o igual al maximo de la cola, entonces X es el maximo
% maximo([X|Xs], Y) :-
%     maximo(Xs, Y),
%     Y > X.  % Si Y es mayor que X, entonces Y es el maximo

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
    cantidadUnidades(Jugador, piquero(_,conEscudo) , CantConEscudo),
    cantidadUnidades(Jugador, piquero(_,sinEscudo) , CantSinEscudo),
    CantConEscudo > CantSinEscudo.

cantidadUnidades(Jugador,Unidad,CantidadUnidad) :-
    unidadesQueTiene(Jugador,Unidades),
    findall(Unidad,member(Unidad,Unidades),ListaUnidades),
    length(ListaUnidades,CantidadUnidad).

%PUNTO 10 ()


dependencia(herreria, [emplumado(punzon),forja(fundicion(horno)),laminas(malla(placas))]).
dependencia(molino, [collera(arado)]).

dependenciaV2(herreria, emplumado).
dependenciaV2(herreria, forja).
dependenciaV2(herreria, laminas).
dependenciaV2(emplumado, punzon).
dependenciaV2(forja, fundicion).
dependenciaV2(fundicion,horno).
dependenciaV2(laminas,malla).
dependenciaV2(malla, placas).
dependenciaV2(molino, collera).
dependenciaV2(collera, arado).

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

esTecnologia(Tecnologia) :- dependenciaV2(Tecnologia,_).
esTecnologia(Tecnologia) :- dependenciaV2(_,Tecnologia).

puedeDesarrollarTecnologia(Jugador, Tecnologia):-
    juega(Jugador,_),
    esTecnologia(Tecnologia),
    not(tecnologiasDesarrolladas(Jugador, Tecnologia)),
    forall(dependenciaV2(TecnologiaRequerida, Tecnologia), tecnologiasDesarrolladas(Jugador, TecnologiaRequerida)).

dependenciaEntre(Tecnologia1, Tecnologia3):- 
    dependenciaV2(Tecnologia1,Tecnologia3).

dependenciaEntre(Tecnologia1, Tecnologia3):-
    dependenciaV2(Tecnologia1, Tecnologia2),
    dependenciaEntre(Tecnologia2, Tecnologia3).


