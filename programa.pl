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

%soldados: campeones, jinetes y piqueros. con distinto nivel cada uno, con o sin escudo.
%defino los tipos de unidades:

unidadesQueTiene(ana, [jinete(caballo),piquero(1,conEscudo),piquero(2,sinEscudo)]).
unidadesQueTiene(beto, [jinete(caballo),piquero(1,conEscudo),campeon(100),campeon(80)]).
unidadesQueTiene(carola, [piquero(3,sinEscudo),piquero(2,conEscudo)]).
unidadesQueTiene(dimitri, []).

campeon(vida):-
    vida >= 1,
    vida =< 100.

jinete(animal):-
    member(animal,[caballo,camello]). %verifico solo si el animal pertenece a la lista [caballo,camello]

piquero(nivel,tieneEscudo):-
    nivel >= 1,
    nivel =< 3,
    member(tieneEscudo,[si,no]).

vidaUnidad(jinete(caballo), 90).
vidaUnidad(jinete(camello), 80).
vidaUnidad(campeon(Vida), Vida).
vidaUnidad(piquero(1,sinEscudo), 50).
vidaUnidad(piquero(2,sinEscudo), 65).
vidaUnidad(piquero(3,sinEscudo), 70).

vidaUnidad(piquero(Nivel,conEscudo), Vida) :-
    vidaUnidad(piquero(Nivel,sinEscudo),VidaSinEscudo),
    Vida is VidaSinEscudo * 1.1. % le sumo el 10%
/*
%VIDAS:
vida_unidad(jugador(Nombre),Unidad,90):-
    jinete(jugador(Nombre),caballo),
    Unidad = jinete.

vida_unidad(jugador(Nombre),Unidad,80):-
    jinete(jugador(Nombre),camello),
    Unidad = jinete.

vida_unidad(jugador(Nombre),Unidad,Vida):-
    campeon(jugador(Nombre),Vida),
    Unidad = campeon.   

%PIQUEROS SIN ESCUDO
vida_unidad(jugador(Nombre),Unidad,Vida):-
    piquero(jugador(Nombre),Nivel,no),
    (Nivel == 1 -> Vida = 50, Nivel == 2 -> Vida = 65, Nivel == 3 -> Vida = 70),
    Unidad = piqueroSinEscudo.

vida_unidad(jugador(Nombre),Unidad,Vida):-
    piquero(jugador(Nombre),Nivel,si),
    (Nivel == 1 -> VidaBase = 50, Nivel == 2 -> VidaBase = 65, Nivel == 3 -> VidaBase = 70).
    Vida = VidaBase * 1.1.
    Unidad = piqueroConEscudo.    
*/

%serviria para conocer la mayor vida de alguna unidad de jugador
vidasUnidadesJugador(Jugador,ListaVidasUnidades):-
    unidadesQueTiene(Jugador,Unidades),
    findall((Unidad,Vida),vidaUnidad(Unidad,Vida),ListaVidasUnidades). 
/*   
unidadConMasVida(jugador(Nombre),Unidad,VidaMax):-
    vidasUnidadesJugador(jugador(Nombre),Vidas),
    max_list(Vidas,(Unidad,VidaMax)).

*/
unidadConMasVida(Jugador,Unidad,VidaMax):-
    juega(Jugador),
    unidadesQueTiene(Jugador,Unidades),
    vidasUnidadesJugador(Jugador,Vidas),
    max_list(Vidas,(Unidad,VidaMax)).




    


