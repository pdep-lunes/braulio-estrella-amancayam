module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = UnPersonaje {
nombre :: String,
poderbasico :: Personaje -> Personaje,
podersuper :: String,
superPoderActivo :: Bool, 
cantidadDeVida :: Int,
}

espina :: Personaje
espina = UnPersonaje {
    nombre = "Espina",
    poderbasico = bolaDeEspina,
    superPoderActivo = undefined,
    superPoderActivo = True,
    cantidadDeVida = 4800,
}
pamela :: Personaje
pamela = UnPersonaje {
    nombre = "Pamela",
    poderbasico = lluviaDeTuercas,
    superPoderActivo = undefined,
    superPoderActivo = False,
    cantidadDeVida = 9600,
}

bolaDeEspina :: Personaje -> Personaje
 
