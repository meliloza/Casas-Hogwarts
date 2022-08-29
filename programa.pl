%   Punto 1) 
%   Saber si una casa permite entrar a un mago, lo cual se cumple para cualquier mago y cualquier
%   casa excepto en el caso de Slytherin, que no permite entrar a magos de sangre impura.
%-------------------------------------------------------------------------------------------------

%definimos casa por Extension
casa(gryffindor).
casa(slytherin).
casa(hufflepuff).
casa(ravenclaw).

% podemos pensar que la sangre es una condicion NECESARIA para ser mago y practicamente mi modelo 
% no funciona si no conzco la sangre de un mago. Asi que no podria insertar un mago a mi programa 
% si no sé su sangre. Entonces hacemos: 

%definimos mago por Compresion (no tiene sentido hacerlo por extension ya que para cada mago tendriamos q hacer su tipo de sangre, es mucho laburo al pe2)

tipoSangre(hermione,impura).
tipoSangre(harry,mestiza).
tipoSangre(draco,pura).

mago(Mago):-
tipoSangre(Mago,_).

%   En principio se puede pensar que si la casa NO es slythering => permite entrar a cualquier mago
%permiteEntrar(Casa, Mago)


%------------------------------------VERSION CON PROBLEMAS DE INVERSIBILIDAD--------------------------------------------------------------------------------------------------------------------
% predicado permiteEntrar() NO es inversible con respecto al Mago xq no tenemos casi info del segundo parametro de permiteEntrar().
%   permiteEntrar(Casa,_):-
%    casa(Casa),                % Aunque no esta implicito en el enunciado, es importante validar que solo existen 4 casas, y no pueden haber otras casas diferentes a las dadas en el enunciado.
%    Casa \= slytheryn.

%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

permiteEntrar(Casa,Mago):-
    casa(Casa), 
    mago(Mago), 
    Casa \= slytheryn.
    
% Puedo pensar que slythering permite entrar a todos los que no tengan sangre impura
permiteEntrar(slytherin,Mago):-
    %mago(Mago), NO es necesario poner este predicado, porque ya el predicado de abajo es inversible.
    tipoSangre(Mago,Sangre), %podemos pensar en un predicado que nos relacione el mago con la sangre
    Sangre \= impura. 


/*-------------------------------------------------------------------------------------------------
    Punto 2) 
    Saber si un mago tiene el carácter apropiado para una casa, lo cual se cumple para cualquier mago
    si sus características incluyen todo lo que se busca para los integrantes de esa casa,
    independientemente de si la casa le permite la entrada.
-------------------------------------------------------------------------------------------------*/

% caracteristicas(Mago, Caracteristicas)
caracteristicas(harry, [coraje, amistad, orgullo, inteligencia]).
caracteristicas(draco, [inteligencia, orgullo]).
caracteristicas(hermione, [inteligencia, orgullo, responsabilidad]).

caracteristicaBuscada(gryffindor, coraje).
caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).
caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(ravenclaw, responsabilidad).
caracteristicaBuscada(hufflepuff, amistad).

/* Version sin abstracciones (no es la mejor)
tieneCaracterApropiado(Mago, Casa):-
  % todas las caracteristicas buscadas por la casa
  % las tiene ese mago
  caracteristicas(Mago, Caracteristicas),
  casa(Casa),
  forall(caracteristicaBuscada(Casa, Caracteristica),
        member(Caracteristica, Caracteristicas)).
*/
tieneCaracteristica(Mago, Caracteristica):-
  caracteristicas(Mago, Caracteristicas),
  member(Caracteristica, Caracteristicas).

tieneCaracterApropiado(Mago, Casa):-
  casa(Casa), mago(Mago),
  forall(caracteristicaBuscada(Casa, Caracteristica),
         tieneCaracteristica(Mago, Caracteristica)).

/*---------------------------------------------------------------------------------------------  
    Punto 3)
    Determinar en qué casa podría quedar seleccionado un mago sabiendo que tiene que tener el
    carácter adecuado para la casa, la casa permite su entrada y además el mago no odiaría que
    lo manden a esa casa. Además Hermione puede quedar seleccionada en Gryffindor, porque al 
    parecer encontró una forma de hackear al sombrero.
-----------------------------------------------------------------------------------------------*/

quedoSeleccionadoEn(Mago, Casa):-
    tieneCaracterApropiado(Mago,Casa),  %como este predicado es innversible, no vamos a necesitar poner los generadores mago() y casa() en el predicado quedoSeleccionadoEn()
    permiteEntrar(Casa,Mago),
    not(odiariaIrA(Mago,Casa)).
quedoSeleccionadoEn(hermione, gryffindor).

%odiariaIrA(Mago,Casa).
odiariaIrA(harry,slytherin).
odiariaIrA(draco,hufflepuff).


/*---------------------------------------------------------------------------------------------  
    Punto 4)
    Definir un predicado cadenaDeAmistades/1 que se cumple para una lista de magos si todos ellos
    se caracterizan por ser amistosos y cada uno podría estar en la misma casa que el siguiente.
    No hace falta que sea inversible, se consultará de forma individual.
-----------------------------------------------------------------------------------------------*/

cadenaDeAmistades(ListaMagos):-
    sonAmistosos(ListaMagos),
    cadenaDeCasas(ListaMagos).

sonAmistosos(ListaMagos):-
    forall(member(Mago,ListaMagos), caracteristicas(Mago,amistad)).


% cadenaDeCasas(Magos)
/* VERSION CON RECURSION */
cadenaDeCasas([Mago1, Mago2 | MagosSiguientes]):-
  quedoSeleccionadoEn(Mago1, Casa),
  quedoSeleccionadoEn(Mago2, Casa),
  cadenaDeCasas([Mago2 | MagosSiguientes]).
cadenaDeCasas([_]).
cadenaDeCasas([]).

puedenQuedarEnLaMismaCasa(Mago1, Mago2, Casa):-
    quedoSeleccionadoEn(Mago1, Casa),
    quedoSeleccionadoEn(Mago2, Casa),
    Mago1 \= Mago2.

/* VERSION SIN RECURSIVIDAD 
cadenaDeCasas(Magos):-
    forall(consecutivos(Mago1, Mago2, Magos),
           puedenQuedarEnLaMismaCasa(Mago1, Mago2, _)).
 
  consecutivos(Anterior, Siguiente, Lista):-
    nth1(IndiceAnterior, Lista, Anterior),
    IndiceSiguiente is IndiceAnterior + 1,
    nth1(IndiceSiguiente, Lista, Siguiente).
*/

%--------------------------------------------------------Parte 2 ------------------------------------------------------------------


  /*1a)
    Saber si un mago es buen alumno, que se cumple si hizo alguna acción y ninguna de las cosas
    que hizo se considera una mala acción (que son aquellas que provocan un puntaje negativo).

*/


%ACCIONES COMO INDIVIDUOS.

hizoAlgunaAccion(Mago):-
    accion(Mago,_). %no nos importa que accion hizo, xq nostros estamos buscando si hizo ALGUNA accion no importa cual.


accion(harry,fueraCama).
accion(hermione, irA(tercerPiso)). %pongo un functor
accion(hermione, irA(seccionRestringidaBiblioteca)).
accion(harry, irA(bosque)).
accion(harry, irA(tercerPiso)).
accion(draco, irA(mazmorras)).
accion(ron, buenaAccion(50, ganarAlAjedrezMagico)).
accion(hermione, buenaAccion(50, salvarASusAmigos)).
accion(harry, buenaAccion(60, ganarleAVoldemort)).

hizoAlgoMalo(Mago):-
    accion(Mago,_).
    puntajeQueGenera(Accion,Puntaje),
    Puntaje <0.

puntajeQueGenera(fueraDeCama, -50).
puntajeQueGenera(irA(Lugar), PuntajeQueResta):-
  lugarProhibido(Lugar, Puntos),
  PuntajeQueResta is Puntos * -1.
puntajeQueGenera(buenaAccion(Puntaje, _), Puntaje).

lugarProhibido(bosque, 50).
lugarProhibido(seccionRestringida, 10).
lugarProhibido(tercerPiso, 75).


esBuenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

/*---------------------------------------------------------------------------------------------  
   
    Punto 1b)
    Saber si una acción es recurrente, que se cumple si más de un mago hizo esa misma acción.

-----------------------------------------------------------------------------------------------*/

esRecurrente(Accion):-      %como modelamos laas acciones como individuos en 1a) entonces esto salio facil.
    accion(Mago, Accion),
    accion(OtroMago, Accion),
    Mago \= OtroMago.

/*---------------------------------------------------------------------------------------------  
   
    Punto 2)
    Saber cuál es el puntaje total de una casa, que es la suma de los puntos obtenidos por 
    sus miembros.

-----------------------------------------------------------------------------------------------*/
%esDe(Persona,Casa).
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).


/* VERSION FEA 
puntajeTotal(Casa,PuntajeTotal):-
    
    esDe(_,Casa),
    findall(PuntajeIndividual,(esDe(Persona,Casa), accion(Mago,Accion), puntajeQueGenera(Accion,PuntajeIndividual)), ListaPuntajes),
    sum_list(ListaPuntajes, PuntajeTotal).

  */  


puntajeTotalDeCasa(Casa, PuntajeTotal):-
    esDe(_, Casa),
    findall(Puntos,
      (esDe(Mago, Casa), puntosQueObtuvo(Mago, _, Puntos)),
      ListaPuntos),
    sum_list(ListaPuntos, PuntajeTotal).
  
  puntosQueObtuvo(Mago, Accion, Puntos):-
    accion(Mago, Accion),
    puntajeQueGenera(Accion, Puntos).

/*---------------------------------------------------------------------------------------------  
   
    Punto 3)
    Saber cuál es la casa ganadora de la copa, que se verifica para aquella casa que haya
    obtenido una cantidad mayor de puntos que todas las otras.

-----------------------------------------------------------------------------------------------*/

casaGanadora(Casa):-
    puntajeTotalDeCasa(Casa, PuntajeMayor),   %podemos decir que el puntaje total de esta casa es el puntaje mayor
    forall((puntajeTotalDeCasa(OtraCasa, PuntajeMenor), Casa \= OtraCasa), PuntajeMayor > PuntajeMenor).
      
    casaGanadora2(Casa):-
    puntajeTotalDeCasa(Casa, PuntajeMayor),
    not((puntajeTotalDeCasa(_, OtroPuntaje), OtroPuntaje > PuntajeMayor)).
    

/*---------------------------------------------------------------------------------------------
    Punto 4)
    Queremos agregar la posibilidad de ganar puntos por responder preguntas en clase.
    La información que nos interesa de las respuestas en clase son: cuál fue la pregunta, 
    cuál es la dificultad de la pregunta y qué profesor la hizo.
-----------------------------------------------------------------------------------------------*/



