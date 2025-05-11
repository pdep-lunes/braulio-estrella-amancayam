module Lib () where
import Text.Show.Functions ()
 
 -- Personajes con: nombre;poder básico;súper poder;si tiene el súper poder activo y su cantidad de vida.

data Personaje = Personaje { --data tipo = constructor {...}
nombre :: [String],
poderbasico :: Personaje -> Personaje,
podersuper :: Personaje -> Personaje,
superPoderActivo :: Bool, 
cantidadDeVida :: Int
}  deriving (Show)

--Modelar a Espina con 4800 puntos de vida, cuyo básico es la bola de espinas y su súper la granada de espinas de 5 metros de radio. ¡Siempre tiene el súper activo!

espina :: Personaje
espina = Personaje {
    nombre = "Espina",
    poderbasico = bolaEspinosa,
    podersuper = granadaDeEspinas 5,
    superPoderActivo = True,
    cantidadDeVida = 4800
}

--Modelar a Pamela con 9600 puntos de vida, cuyo básico es la lluvia de tuercas sanadoras y el súper la torreta curativa (full soporte). No tiene el súper activo

pamela :: Personaje
pamela = Personaje {
    nombre = "Pamela",
    poderbasico = lluviaDeTuercas "sanadora",
    podersuper = torretaCurativa,
    superPoderActivo = False,
    cantidadDeVida = 9600
}

listaDePersonajes :: [Personaje]
listaDePersonajes = [espina , pamela]

--bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).
--como no puede quedar negativo uso max 0 que devuelve el mayor, por lo que si llega a quedar negativo va a devolver 0

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = personaje { cantidadDeVida = max 0 (cantidadDeVida personaje - 1000)} 
--ejemplo: bolaEspinosa pamela = pamela {.... cantidadDeVida = 8600}

-- lluviaDeTuercas: pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y 
--las segundas le disminuyen a la mitad la vida de quien sea su contrincante. En cualquier otro caso, no le pasa nada al personaje.

lluviaDeTuercas :: String -> Personaje -> Personaje --Toma si es sanadora o dañina y el personaje
-- yo asumo que el usuario ingresa por teclado sanadora / danina y que ingresa el colega o el contrincante segun corresponda
lluviaDeTuercas poder personaje 
    |poder == "sanadora" = personaje {cantidadDeVida = cantidadDeVida + 800}
    |poder == "danina" = personaje {cantidadDeVida = div (cantidadDeVida personaje) 2}
    |otherwise = personaje
--ejemplo :  lluviaDeTuercas "sanadora" espina = espina {... cantidadDeVida = 5600}

--granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su nombre “Espina estuvo aquí”. 
--Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. En otro caso, se usa una bola de espinas.

granadaDeEspinas :: Int -> Personaje -> Personaje --Toma el Radio y el contrincante 
granadaDeEspinas radio contrincante
    |radio > 3 && contrincante pocaVida = contrincante {cantidadDeVida = 0, superPoderActivo = False } -- no se si en este caso tambien se le agrega "Espina estuvo aqui"
    |radio > 3 = contrincante {nombre = nombre contrincante ++ "Espina estuvo aquí"} 
    |otherwise = bolaEspinosa contrincante
--ejemplo : granadaDeEspinas 5 pamela = pamela = {nombre : "Pamela Espina estuvo aqui"...}

pocaVida :: Personaje -> Bool
pocaVida personaje = cantidadDeVida personaje < 800 

--torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.
torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = aliado {superPoderActivo = True, cantidadDeVida = cantidadDeVida aliado * 2}
--ejemplo: torretaCurativa espina = {...superPoderActivo = true, cantidadDeVida = 9600}

--Además se quiere reportar lo siguiente:
{- atacar con el poder especial: si el personaje tiene el súper poder activo, entonces va a atacar a su contrincante con el súper y con el básico. Si no, no hará nada.
saber quiénes están en las últimas: es decir, el nombre de aquellos brawlers que tienen menos de 800 puntos de vida.
-}

atacarConPoderEspecial:: Personaje -> Personaje -> Personaje
--ingresa el personaje y el contrincante
atacarConPoderEspecial personaje contrincante
    |superPoderActivo personaje contrincante = ( podersuper personaje . poderbasico personaje) contricante
    |otherwise = contrincante 
-- atacarConPoderEspecial espina pamela = Personaje {nombre = "Pamela Espina estuvo aqui", superPoderActivo = False, cantidadDeVida = 8600}

quienEstaenlasUltimas :: [Personaje] -> [String] --Lista de todos los nombres de brawlers que tengan menos de 800 de vida
quienEstaenlasUltimas personajes = map nombre (filter pocaVida personajes)--filter :: (a->Bool) -> [a] -> [a]
--quienEstaenlasUltimas listaDePersonajes = [] en este caso porque ninguno tiene poca vida











