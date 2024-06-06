---------------------------------------
--------------- Punto 1 ---------------
---------------------------------------

data Personaje = Personaje{
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show)

-- type Habilidad = String

data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema]
}

data Universo = Universo{
    habitantes :: [Personaje]
}

chasquidoDelUniverso :: Guantelete -> Universo -> Universo
chasquidoDelUniverso unGuantelete unUniverso
    | guanteleteEstaCompleto unGuantelete = reducirUniversoALaMitad unUniverso
    | otherwise = unUniverso

guanteleteEstaCompleto :: Guantelete -> Bool
guanteleteEstaCompleto unGuantelete = material unGuantelete == "uru" && length (gemas unGuantelete) == 6

reducirUniversoALaMitad :: Universo -> Universo
reducirUniversoALaMitad unUniverso = modificarUniverso mitadDeHabitantes unUniverso
    where
        personajes = habitantes unUniverso
        mitadDeHabitantes = take (length personajes `div` 2) personajes

modificarUniverso :: [Personaje] -> Universo -> Universo
modificarUniverso listaHabitantesNueva unUniverso = unUniverso {habitantes = listaHabitantesNueva}

---------------------------------------
--------------- Punto 2 ---------------
---------------------------------------

universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex unUniverso = hayMenoresDe45 (habitantes unUniverso)

hayMenoresDe45 :: [Personaje] -> Bool
hayMenoresDe45 listaHabitantes = any (< 45) . map edad $ listaHabitantes

energiaTotalDeUniverso :: Universo -> Int
energiaTotalDeUniverso unUniverso = sumarEnergias . habitantesConMasDeUnaHabilidad $ (habitantes unUniverso)

habitantesConMasDeUnaHabilidad :: [Personaje] -> [Personaje]
habitantesConMasDeUnaHabilidad listaHabitantes = filter ((> 1) . length . habilidades) listaHabitantes
                                                 
sumarEnergias :: [Personaje] -> Int
sumarEnergias listaHabitantesHabiles = sum . map energia $ listaHabitantesHabiles

---------------------------------------
--------------- Punto 3 ---------------
---------------------------------------

type Gema = Personaje -> Personaje

--1)

laMente :: Int -> Gema
laMente unValor unPersonaje = quitarEnergia unValor unPersonaje

quitarEnergia :: Int -> Personaje -> Personaje
quitarEnergia valorAQuitar unPersonaje = modificarEnergia (energia unPersonaje - valorAQuitar) unPersonaje

modificarEnergia :: Int -> Personaje -> Personaje
modificarEnergia nuevaEnergia unPersonaje = unPersonaje {energia = nuevaEnergia}

-- 2)

elAlma :: String -> Gema
elAlma unaHabilidad unPersonaje = quitarEnergia 10 . quitarHabilidad unaHabilidad $ unPersonaje

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad unaHabilidad unPersonaje = modificarHabilidades habilidadesDiferentes unPersonaje
    where 
        habilidadesDiferentes = filter (/= unaHabilidad) (habilidades unPersonaje)

modificarHabilidades :: [String] -> Personaje -> Personaje
modificarHabilidades listaHabilidadesNueva unPersonaje = unPersonaje {habilidades = listaHabilidadesNueva}

-- 3)

elEspacio :: String -> Gema
elEspacio unPlaneta unPersonaje = quitarEnergia 20 . modificarPlaneta unPlaneta $ unPersonaje

modificarPlaneta :: String -> Personaje -> Personaje 
modificarPlaneta planetaNuevo unPersonaje = unPersonaje {planeta = planetaNuevo}

-- 4)

elPoder :: Gema
elPoder unPersonaje = quitarEnergia (energia unPersonaje) . quitarHabilidades $ unPersonaje

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades unPersonaje
    | (< 3) . length . habilidades $ unPersonaje = modificarHabilidades [] unPersonaje
    | otherwise = unPersonaje

-- 5)

elTiempo :: Gema
elTiempo unPersonaje = corroborarQueSeaMayor . dividirEdad $ unPersonaje

dividirEdad :: Personaje -> Personaje
dividirEdad unPersonaje = modificarEdad ((edad unPersonaje) `div` 2) unPersonaje

modificarEdad :: Int -> Personaje -> Personaje
modificarEdad edadNueva unPersonaje = unPersonaje {edad = edadNueva}

corroborarQueSeaMayor :: Personaje -> Personaje
corroborarQueSeaMayor unPersonaje
    | edad unPersonaje >= 18 = unPersonaje
    | otherwise = modificarEdad 18 unPersonaje

-- 6) 

gemaLoca :: Gema -> Personaje -> Personaje
gemaLoca unaGema unpersonaje  = unaGema . unaGema $ unpersonaje

---------------------------------------
--------------- Punto 4 ---------------
---------------------------------------

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete{
    material = "",
    gemas = [elTiempo, elAlma "usar Mjolnir", gemaLoca (elAlma "programacion en Haskell")]
}

---------------------------------------
--------------- Punto 5 ---------------
---------------------------------------

utilizarGemas :: [Gema] -> Personaje -> Personaje 
utilizarGemas listaGemas unPersonaje = foldr aplicarGema unPersonaje listaGemas
 
-- El efecto de lado se produce comenzando por la última gema de la lista sobre el personaje y se sigue aplicando hacia la izquierda sobre el personaje con los valores actualizados

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema unaGema unPersonaje = unaGema unPersonaje

---------------------------------------
--------------- Punto 6 ---------------
---------------------------------------

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje = gemaQueQuitaMasEnergia unPersonaje (gemas unGuantelete)

gemaQueQuitaMasEnergia :: Personaje -> [Gema] -> Gema
gemaQueQuitaMasEnergia _ [unaGema] = unaGema
gemaQueQuitaMasEnergia unPersonaje (unaGema : otraGema : masGemas)
    | energia (unaGema unPersonaje) < energia (otraGema unPersonaje) = gemaQueQuitaMasEnergia unPersonaje (unaGema : masGemas)
    | otherwise = gemaQueQuitaMasEnergia unPersonaje (otraGema : masGemas)

---------------------------------------
--------------- Punto 7 ---------------
---------------------------------------

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizarGemas . take 3. gemas) guantelete

punisher :: Personaje
punisher = Personaje{
    edad = 30,
    energia = 1000,
    habilidades = ["Ser muy picante", "Estar muy chasiado", "Ser enrome"],
    nombre = "Punisher",
    planeta = "Tierra"
}

-- El resultado esperado sería este:
-- Personaje {
    -- edad = 18, 
    -- energia = 1000, 
    -- habilidades = ["Ser muy picante","Estar muy chasiado","Ser enrome"], 
    -- nombre = "Punisher", 
    -- planeta = "Tierra"
-- }

-- El resultado sí se podrá dar, debido a que lo que hace la función es aplicar lo que hace la gema que está en la lista infinita 3 veces. Esto lo hace tomando las 3 primeras gemas de la lista infinita y de ahí las aplica como una lista de 3 gemas iguales.
-- Esto sí puede suceder, debido que en el momento en el que uso "infinitasGemas", no estoy obteniendo una lista infinita de gemas instantáneamente, sino que debido al concepto de "lazy evaluation", los valores no se asignan hasta que es totalmente necesario para su utilización.
-- Es por esto que al hacer un "take 3" de la lista infinita, la lista va generando esas 3 gemas a medida que se las piden, y es por eso que puede devolverlas y funcionar correctamente.