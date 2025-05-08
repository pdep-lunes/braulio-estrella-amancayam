module Lib () where
import Text.Show.Functions ()
 
 -- Personajes con: nombre;poder básico;súper poder;si tiene el súper poder activo y su cantidad de vida.

data Personaje = Personaje { --data tipo = constructor
nombre :: [String],
poderbasico :: Personaje -> Personaje, --dios sabra
podersuper :: String,
superPoderActivo :: Bool, 
cantidadDeVida :: Int,
}

--Modelar a Espina con 4800 puntos de vida, cuyo básico es la bola de espinas y su súper la granada de espinas de 5 metros de radio. ¡Siempre tiene el súper activo!

espina :: Personaje
espina = Personaje {
    nombre = "Espina",
    poderbasico = bolaDeEspina,
    podersuper = undefined,
    superPoderActivo = True,
    cantidadDeVida = 4800,
}

--Modelar a Pamela con 9600 puntos de vida, cuyo básico es la lluvia de tuercas sanadoras y el súper la torreta curativa (full soporte). No tiene el súper activo

pamela :: Personaje
pamela = Personaje {
    nombre = "Pamela",
    poderbasico = lluviaDeTuercas,
    podersuper = undefined,
    superPoderActivo = False,
    cantidadDeVida = 9600,
}

--bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).
--como no puede quedar negativo uso max 0 que devuelve el mayor, por lo que si llega a quedar negativo va a devolver 0

bolaDeEspina :: Personaje -> Personaje
bolaDeEspina personaje = personaje { cantidadDeVida = max 0 (cantidadDeVida personaje - 1000)} 

 -- lluviaDeTuercas: pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y 
 --las segundas le disminuyen a la mitad la vida de quien sea su contrincante. En cualquier otro caso, no le pasa nada al personaje.
lluviaDeTuercas :: String -> Personaje -> Personaje --Toma si es sanadora o dañina y el personaje
-- yo asumo que el usuario ingresa por teclado sanadora / danina y que ingresa el colega o el contrincante segun corresponda
lluviaDeTuercas poder personaje 
    |poder == "sanadora" = personaje {cantidadDeVida = cantidadDeVida + 800}
    |poder == "danina" = personaje {cantidadDeVida = cantidadDeVida * 0.5}
    |otherwise = personaje
 
--granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su nombre “Espina estuvo aquí”. 
--Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. En otro caso, se usa una bola de espinas.

granadaDeEspinas :: Int -> Personaje -> Personaje --Toma el Radio y el contrincante 
granadaDeEspinas radio contrincante
    |radio > 3 = contrincante {nombre = nombre contrincante ++ "Espina estuvo aquí"} 
    |radio > 3 && contrincante pocaVida = contrincante {cantidadDeVida = 0, superPoderActivo == False}
    |otherwise = bolaDeEspina contrincante

pocaVida :: Personaje -> Bool
pocaVida personaje = cantidadDeVida personaje < 800 --esta bien hacer una funcion asi de tonta ? o lo hago en la otra?

--torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.
torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = aliado {superPoderActivo = True, salud = salud aliado * 2}
