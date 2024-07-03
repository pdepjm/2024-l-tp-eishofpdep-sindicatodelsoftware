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


