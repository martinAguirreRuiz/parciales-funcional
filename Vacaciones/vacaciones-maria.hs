import Data.List (genericLength)
import Text.Show.Functions


data Turista = Turista {
    cansancio :: Float,
    estres :: Float,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving(Show, Eq)

type Idioma = String


-----------------------------------------
-----------------Punto 1-----------------
-----------------------------------------

ana :: Turista
ana = Turista {
    cansancio = 0,
    estres = 21,
    viajaSolo = False,
    idiomas = ["Espaniol"]
}

betho :: Turista
betho = Turista {
    cansancio = 15,
    estres = 15,
    viajaSolo = True,
    idiomas = ["Aleman"]
}

cathi :: Turista
cathi = Turista {
    cansancio = 15,
    estres = 15,
    viajaSolo = True,
    idiomas = ["Aleman", "Catalan"]
}

-----------------------------------------
-----------------Punto 2-----------------
-----------------------------------------

type Excursion = Turista -> Turista

-----------------2.a-----------------

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = modificarCansancio (-) 5 unTurista
    | otherwise = modificarEstres (-) 1 unTurista

modificarCansancio :: (Float ->Float ->Float) -> Float -> Turista -> Turista
modificarCansancio operacion valor unTurista = unTurista {cansancio = operacion (cansancio unTurista) valor}

modificarEstres :: (Float ->Float ->Float) -> Float -> Turista -> Turista
modificarEstres operacion valor unTurista = unTurista {estres = operacion (estres unTurista) valor}

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje unElemento = modificarEstres (-) (genericLength unElemento)

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma unIdioma unTurista = anadirIdioma unIdioma unTurista {viajaSolo = False}

anadirIdioma :: String -> Turista -> Turista
anadirIdioma unIdioma unTurista = unTurista {idiomas = unIdioma : idiomas unTurista}

caminar :: Float -> Excursion
caminar unosMinutos = modificarEstres (-) nivelIntensidad . modificarCansancio (+) nivelIntensidad
    where nivelIntensidad = unosMinutos / 4

pasearEnBarco :: Marea -> Excursion
pasearEnBarco Tranquila = caminar 10 . apreciarElementoDelPaisaje "Mar" . salirAHablarUnIdioma "Aleman"
pasearEnBarco Fuerte = modificarEstres (+) 6 . modificarCansancio (+) 10
pasearEnBarco Moderada = id
   

data Marea = Fuerte | Tranquila | Moderada deriving(Show, Eq)

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion unaExcursion unTurista = unaExcursion . modificarEstres (-) (estres unTurista * 0.1) $ unTurista

-----------------2.b-----------------

deltaSegun :: (a -> Float) -> a -> a -> Float
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
-- Por ejemplo, si “stress” es la función que me da el stress de un turista:
-- > deltaExcursionSegun stress ana irALaPlaya
-- -3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20)

type Indice = Turista -> Float

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Float
deltaExcursionSegun unIndice unTurista unaExcursion = deltaSegun unIndice (hacerUnaExcursion unaExcursion unTurista) unTurista

-----------------2.c-----------------
esEducativa :: Excursion -> Turista -> Bool
esEducativa unaExcursion unTurista = deltaExcursionSegun (genericLength.idiomas) unTurista unaExcursion > 0

esDesestresante :: Excursion -> Turista -> Bool
esDesestresante unaExcursion unTurista = deltaExcursionSegun estres unTurista unaExcursion <= -3 


-----------------------------------------
-----------------Punto 3-----------------
-----------------------------------------

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElementoDelPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Tour
ladoB = [pasearEnBarco Tranquila, caminar 120] 

hacerUnTour :: Tour -> Turista -> Turista
hacerUnTour unTour unTurista = foldr hacerUnaExcursion unTurista unTour

esConvincente :: Tour -> Turista -> Bool
esConvincente unTour unTurista = any (\unaExcursion -> esDesestresante unaExcursion unTurista && dejaAcompaniado unaExcursion unTurista) unTour

dejaAcompaniado :: Excursion -> Turista -> Bool
dejaAcompaniado unaExcursion = not . viajaSolo .hacerUnaExcursion unaExcursion


-- Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour. 
-- La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.

efectividadDeUnTour :: Tour -> [Turista] -> Float
efectividadDeUnTour unTour =  sum . espiritualidadDeLosTuristas unTour . filter (esConvincente unTour) 

espiritualidadDeLosTuristas :: Tour -> [Turista] -> [Float]
espiritualidadDeLosTuristas unTour = map (espiritualidadDeUnTurista unTour)

espiritualidadDeUnTurista :: Tour -> Turista -> Float
espiritualidadDeUnTurista unTour unTurista = ( estres unTurista - estresPostTour) + (cansancio unTurista - cansancioPostTour)
    where cansancioPostTour = cansancio (hacerUnTour unTour unTurista)
          estresPostTour = estres (hacerUnTour unTour unTurista)

