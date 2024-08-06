% Aquí va el código.

%1era entrega:

%PUNTO 1:
jugador(ana).
jugador(beto).
jugador(carola).
jugador(dimitri).

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
    jugador(Jugador),
    tecnologiasDesarrolladas(Jugador,herreria),
    tecnologiasDesarrolladas(Jugador,forja),
    juega(Jugador,romanos).

esExpertoEnMetales(Jugador):-
    jugador(Jugador),
    tecnologiasDesarrolladas(Jugador,herreria),
    tecnologiasDesarrolladas(Jugador,forja),
    tecnologiasDesarrolladas(Jugador,fundicion).

/*
desarrolló herrería && desarrolló forja && (desarrolló fundición || juega con los romanos)
*/

%PUNTO 3:
civilizacionPopular(Civilizacion):-
    civilizacion(Civilizacion),
    juega(Jugador1,Civilizacion),
    juega(Jugador2,Civilizacion),
    Jugador1 \= Jugador2.

%PUNTO 4:
tieneAlcanceGlobal(Tecnologia):-
    tecnologia(Tecnologia),
    forall(jugador(Jugador),tecnologiasDesarrolladas(Jugador,Tecnologia)).

%forall(universoAcomprobar,dondeSeCompruebaCadaElementoDelUniverso).

%PUNTO 5:
civilizacionEsLider(Civilizacion):-
    civilizacion(Civilizacion),
    forall(tecnologia(Tecnologia),civilizacionAlcanzoUnaTecnologia(Civilizacion,Tecnologia)).    

civilizacionAlcanzoUnaTecnologia(Civilizacion,Tecnologia):-
    juega(Jugador,Civilizacion),
    tecnologiasDesarrolladas(Jugador,Tecnologia).

%PRIMERA ENTREGA TERMINADA

% Unidades de Ana
unidad(ana, jinete(caballo)).
unidad(ana, piquero(nivel(1), escudo)).
unidad(ana, piquero(nivel(2), sinEscudo)).

% Unidades de Beto
unidad(beto, campeon(100)).
unidad(beto, campeon(80)).
unidad(beto, piquero(nivel(1), escudo)).
unidad(beto, jinete(camello)).

% Unidades de Carola
unidad(carola, piquero(nivel(3), sinEscudo)).
unidad(carola, piquero(nivel(2), escudo)).

% Dimitri no tiene unidades


vidaUnidad(campeon(Vida), Vida).
vidaUnidad(jinete(caballo), 90).
vidaUnidad(jinete(camello), 80).
vidaUnidad(piquero(nivel(1), sinEscudo), 50).
vidaUnidad(piquero(nivel(2), sinEscudo), 65).
vidaUnidad(piquero(nivel(3), sinEscudo), 70).
vidaUnidad(piquero(nivel(N), escudo), Vida) :-
    vidaUnidad(piquero(nivel(N), sinEscudo), VidaBase),
    Vida is VidaBase * 1.1.


unidadConMasVida(Jugador, Unidad) :-
    findall((Vida,UnidadActual), (unidad(Jugador, UnidadActual), vidaUnidad(UnidadActual, Vida)), VidasUnidades),
    max_member(Unidad, VidasUnidades).

%PUNTO 8:
ventaja(jinete(_),campeon(_)).
ventaja(campeon(_),piquero(_,_)).
ventaja(piquero(_,_),jinete(_)).
ventaja(jinete(camello),jinete(caballo)).

unidadLeGanaA(Unidad1,Unidad2):-
    ventaja(Unidad1,Unidad2).

unidadLeGanaA(Unidad1,Unidad2):-
    not(ventaja(Unidad1,Unidad2)),
    mayorVidaGana(Unidad1,Unidad2).

mayorVidaGana(Unidad1,Unidad2):-
    vidaUnidad(Unidad1,Vida1),
    vidaUnidad(Unidad2,Vida2),
    Vida1 > Vida2.

sobreviveAlAsedio(Jugador):-
    findall(piquero(_,escudo),unidad(Jugador,piquero(_,escudo)),UnidadesConEscudo),
    findall(piquero(_,sinEscudo),unidad(Jugador,piquero(_,sinEscudo)),UnidadesSinEscudo),
    length(UnidadesConEscudo,LCE),
    length(UnidadesSinEscudo,LSE),
    LCE > LSE.

