import Data.List (genericLength)
import Text.Show.Functions

--------------------------------
-----------  Punto 1 -----------
--------------------------------


data Turista = Turista{
    cansancio :: Float,
    estres :: Float,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
}deriving(Show, Eq)

type Idioma = String

ana :: Turista
ana = Turista{
    cansancio = 0,
    estres = 21,
    viajaSolo = False,
    idiomas = ["Espaniol"]
}

beto :: Turista 
beto = Turista{
    cansancio = 15,
    estres = 15,
    viajaSolo = True,
    idiomas = ["Aleman"]
}

cathi :: Turista 
cathi = Turista{
    cansancio = 15,
    estres = 15,
    viajaSolo = True,
    idiomas = ["Aleman", "Catalan"]
}

--------------------------------
-----------  Punto 2 -----------
--------------------------------

type Excursion = Turista -> Turista

irALaPlaya :: Turista -> Turista
irALaPlaya unTurista 
    | viajaSolo unTurista = quitarCansancio 5 unTurista
    | otherwise = quitarEstres 1 unTurista

quitarEstres :: Float -> Turista -> Turista
quitarEstres valorAQuitar unTurista = modificarEstres (estres unTurista - valorAQuitar) unTurista

quitarCansancio :: Float -> Turista -> Turista
quitarCansancio valorAQuitar unTurista = modificarCansancio (cansancio unTurista - valorAQuitar) unTurista

modificarCansancio :: Float -> Turista -> Turista
modificarCansancio nuevoCansancio unTurista = unTurista {cansancio = nuevoCansancio}

modificarEstres :: Float -> Turista -> Turista
modificarEstres nuevoEstres unTurista = unTurista {estres = nuevoEstres}

apreciarElementoDelPaisaje :: Elemento -> Turista -> Turista
apreciarElementoDelPaisaje unElementoDelPaisaje unTurista = quitarEstres (cantidadDeLetrasElemento) unTurista
    where 
        cantidadDeLetrasElemento = genericLength unElementoDelPaisaje

type Elemento = String

salirAHablarUnIdioma :: Idioma -> Turista -> Turista
salirAHablarUnIdioma unIdioma unTurista = agregarIdioma unIdioma unTurista

agregarIdioma :: Idioma -> Turista -> Turista
agregarIdioma nuevoIdioma unTurista = unTurista {idiomas = nuevoIdioma : idiomas unTurista}

-- modificarIdioma :: Idioma -> Turista -> Turista
-- modificarIdioma nuevoIdioma unTurista = unTurista {idiomas = nuevoIdioma : idiomas unTurista}

caminarCiertosMinutos :: Float -> Turista -> Turista
caminarCiertosMinutos tiempoCaminado unTurista = quitarEstres intensidad . quitarCansancio intensidad $ unTurista
    where 
        intensidad = tiempoCaminado / 4

paseoEnBarco :: Marea -> Turista -> Turista
paseoEnBarco Fuerte unTurista = aumentarEstres 6 . aumentarCansancio 10 $ unTurista
paseoEnBarco Moderada unTurista = unTurista
paseoEnBarco Tranquila unTurista = salirAHablarUnIdioma "Aleman" . apreciarElementoDelPaisaje "Mar" . caminarCiertosMinutos 10 $ unTurista

data Marea = Fuerte | Moderada | Tranquila

aumentarEstres :: Float -> Turista -> Turista
aumentarEstres valorAAgregar unTurista = modificarEstres (estres unTurista + valorAAgregar) unTurista

aumentarCansancio :: Float -> Turista -> Turista
aumentarCansancio valorAAgregar unTurista = modificarCansancio (estres unTurista + valorAAgregar) unTurista

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion unaExcursion unTurista = quitarEstres (estres unTurista * 0.1) . unaExcursion $ unTurista

deltaSegun :: (a -> Float) -> a -> a -> Float
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: Indice -> Excursion -> Turista -> Float
deltaExcursionSegun unIndice unaExcursion unTurista = deltaSegun unIndice (hacerUnaExcursion unaExcursion unTurista) unTurista

type Indice = Turista -> Float

excursionEsEducativa :: (Turista -> [Idioma]) -> Excursion -> Turista -> Bool
excursionEsEducativa fIdioma unaExcursion unTurista = (deltaExcursionSegun (genericLength . fIdioma) unaExcursion unTurista) > 0

excursionEsDesestresante :: Indice -> Turista -> Excursion -> Bool
excursionEsDesestresante fEstres unTurista unaExcursion = (deltaExcursionSegun fEstres unaExcursion unTurista) <= (- 3)

--------------------------------
-----------  Punto 3 -----------
--------------------------------

type Tour = [Excursion]

completo :: Tour
-- completo = [caminarCiertosMinutos 20, apreciarElementoDelPaisaje "cascada", caminarCiertosMinutos 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]
completo = [salirAHablarUnIdioma "melmacquiano", irALaPlaya, caminarCiertosMinutos 40, apreciarElementoDelPaisaje "cascada", caminarCiertosMinutos 20]

ladoB :: Excursion -> Tour
ladoB unaExcursion = [caminarCiertosMinutos 120 , unaExcursion , paseoEnBarco Tranquila]

islaVecina :: Excursion -> Marea -> Tour
islaVecina unaExcursion Fuerte = [paseoEnBarco Fuerte, unaExcursion, paseoEnBarco Fuerte]
islaVecina unaExcursion Moderada = [paseoEnBarco Moderada, unaExcursion, paseoEnBarco Moderada]
islaVecina unaExcursion Tranquila = [paseoEnBarco Tranquila, unaExcursion, paseoEnBarco Tranquila]

hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour unTurista unTour =  foldr hacerUnaExcursion (aumentarEstres (genericLength unTour) unTurista) unTour

tourEsConvincente :: Turista -> [Tour] -> Bool
tourEsConvincente unTurista listaTours = hayTourDejaAcompaniado unTurista . toursDesestresantes unTurista $ listaTours

hayTourDejaAcompaniado :: Turista -> [Tour] -> Bool
hayTourDejaAcompaniado unTurista = any (not. viajaSolo . hacerUnTour unTurista)

toursDesestresantes :: Turista -> [Tour] -> [Tour]
toursDesestresantes unTurista listaTours = filter (tourEsDesestresante unTurista) listaTours

tourEsDesestresante :: Turista -> Tour -> Bool
tourEsDesestresante unTurista unTour = any (excursionEsDesestresante estres unTurista) unTour

efectividadDeUnTour :: [Turista] -> Tour -> Float
efectividadDeUnTour [] _ = 0
efectividadDeUnTour (unTurista:otrosTuristas) unTour = espiritualidadRecibida unTurista [unTour] + efectividadDeUnTour otrosTuristas unTour

espiritualidadRecibida :: Turista -> [Tour] -> Float
espiritualidadRecibida unTurista unTour 
    | tourEsConvincente unTurista unTour = perdidaEstresYCansancio unTurista unTour
    | otherwise = 0

perdidaEstresYCansancio :: Turista -> [Tour] -> Float
perdidaEstresYCansancio unTurista (unTour:otrosTours) = perdidaEstres unTour unTurista + perdidaCansancio unTour unTurista

perdidaEstres :: Tour -> Turista -> Float
perdidaEstres unTour unTurista = estres unTurista - estres (hacerUnTour unTurista unTour) 

perdidaCansancio :: Tour -> Turista -> Float
perdidaCansancio unTour unTurista = cansancio unTurista - cansancio (hacerUnTour unTurista unTour)

--------------------------------
-----------  Punto 4 -----------
--------------------------------

visitarInifinitasPlayas :: Tour
visitarInifinitasPlayas = repeat irALaPlaya

-- b) No se puede saber si es convincente visitarInfinitasPlayas, porque toursDesestresantes se quedará filtrando la lista hasta el infinito, entonces nunca va a poder devolver la lista completa, y por ende se queda tildado ahí queriendo hacer eso

-- c) Para poder saber la efectividad de un tour, ese tour tiene que ser convincente, por lo que, al no poder verificar el punto anterior, entonces nunca podré saber su efectividad.