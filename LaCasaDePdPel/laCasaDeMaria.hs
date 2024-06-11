-- Alumna = {
--     nombre = "Maria",
--     apellido = "Musante",
--     legajo = 211.795-2
-- }

import Data.List (genericLength, find)
import Text.Show.Functions


data Ladron = Ladron {
    nombre :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
} deriving (Show)

type Habilidad = String

data Rehen = Rehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    planes :: [Plan]
} deriving (Show)



type Arma = Rehen -> Rehen

pistola :: Int -> Arma
pistola unCalibre unRehen = modificarNivelComplot (- 5 * unCalibre) . modificarNivelMiedo ((3 *) . contarLetrasNombreRehen $ unRehen) $ unRehen

contarLetrasNombreRehen :: Rehen -> Int
contarLetrasNombreRehen = length . nombreRehen

ametralladora :: Int -> Arma
ametralladora cantBalas unRehen = modificarNivelComplot (flip div (-2) . nivelComplot $ unRehen) . modificarNivelMiedo cantBalas $ unRehen 

modificarNivelComplot :: Int -> Rehen -> Rehen
modificarNivelComplot unaCantidad unRehen = unRehen {nivelComplot = nivelComplot unRehen + unaCantidad}

type Intimidacion = Ladron -> Rehen -> Rehen


dispararAlTecho :: Intimidacion
dispararAlTecho unLadron unRehen = armaQueMasMiedoGenera unRehen unLadron unRehen

armaQueMasMiedoGenera :: Rehen -> Ladron -> Arma
armaQueMasMiedoGenera unRehen unLadron = foldr (compararArmas unRehen) (head $ armas unLadron) (tail $ armas unLadron)

compararArmas :: Rehen -> Arma -> Arma -> Arma
compararArmas unRehen unArma otroArma
    | (nivelMiedo (unArma unRehen) - nivelMiedo unRehen) > (nivelMiedo (otroArma unRehen) - nivelMiedo unRehen) = unArma
    | otherwise = otroArma

hacerseElMalo :: Intimidacion
hacerseElMalo unLadron unRehen
    | nombre unLadron == "Berlin" = modificarNivelMiedo (contarLetrasDeHabilidades unLadron) unRehen
    | nombre unLadron == "Rio" = modificarNivelComplot 20 unRehen
    | otherwise = modificarNivelMiedo 10 unRehen

modificarNivelMiedo :: Int -> Rehen -> Rehen
modificarNivelMiedo unaCantidad unRehen = unRehen {nivelMiedo = nivelMiedo unRehen + unaCantidad}

contarLetrasDeHabilidades :: Ladron -> Int
contarLetrasDeHabilidades = sum . map length . habilidades

type Plan = Rehen -> Ladron -> Ladron

atacarLadron :: Rehen -> Plan
atacarLadron unCompaRehen _ = quitarArmas (flip div 10 $ contarLetrasNombreRehen unCompaRehen)

quitarArmas :: Int -> Ladron -> Ladron
quitarArmas unaCantidad unLadron = unLadron{ armas = drop unaCantidad (armas unLadron)}

esconderse :: Plan
esconderse _ unLadron = flip quitarArmas unLadron . flip div 3 . contarHabilidadesUnLadron $ unLadron  

contarHabilidadesUnLadron :: Ladron -> Int
contarHabilidadesUnLadron = length . habilidades


-----------------------------------------
----------------Punto 1------------------
-----------------------------------------

tokio :: Ladron
tokio = Ladron {
    nombre = "Tokio",
    habilidades = ["trabajo psicologico", "entrar en moto"],
    armas = [pistola 9, pistola 9, ametralladora 30]
}
profesor :: Ladron
profesor = Ladron {
    nombre = "Profesor",
    habilidades = ["disfrazarse de linyera", "disfrazarse de payaso" , "estar siempre un paso adelante"],
    armas = [pistola 9, pistola 9, ametralladora 30]
}

pablo :: Rehen
pablo = Rehen {
    nombreRehen = "Pablo",
    nivelComplot = 40,
    nivelMiedo = 30,
    planes = [esconderse]
}

arturito :: Rehen
arturito = Rehen {
    nombreRehen = "Arturito",
    nivelComplot = 70,
    nivelMiedo = 50,
    planes = [esconderse, atacarLadron pablo]
}

-----------------------------------------
----------------Punto 2------------------
-----------------------------------------

esInteligente :: Ladron -> Bool
esInteligente unLadron = nombre unLadron == "Profesor" || contarHabilidadesUnLadron unLadron > 2

conseguirNuevaArma :: Arma -> Ladron -> Ladron
conseguirNuevaArma unArma unLadron = unLadron {armas = unArma : armas unLadron}

intimidarUnRehen :: Intimidacion -> Ladron -> Rehen -> Rehen
intimidarUnRehen unaIntimidacion = unaIntimidacion


-- 5. Que un ladrón calme las aguas, disparando al techo frente a un grupo de rehenes, de los cuales se
-- calman los que tengan más de 60 de complot

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas unLadron unosRehenes = filter (not . nivelDeComplotMayorA60) unosRehenes ++ (dispararAlTechoFrenteAVarios unLadron . filter nivelDeComplotMayorA60 $ unosRehenes)

nivelDeComplotMayorA60 :: Rehen -> Bool
nivelDeComplotMayorA60 = (> 60) . nivelComplot

dispararAlTechoFrenteAVarios :: Ladron -> [Rehen] -> [Rehen]
dispararAlTechoFrenteAVarios unLadron = map (dispararAlTecho unLadron)

puedeEscaparseDeLaPolicia :: Ladron -> Bool
puedeEscaparseDeLaPolicia unLadron = any (tieneHabilidadDeDisfrazar unLadron) (habilidades unLadron)

tieneHabilidadDeDisfrazar :: Ladron -> Habilidad -> Bool
tieneHabilidadDeDisfrazar unLadron unaHabilidad = take 14 unaHabilidad == "disfrazarse de"

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal unosLadrones unosRehenes = nivelAlgoPromedio nivelComplot unosRehenes > nivelAlgoPromedio nivelMiedo unosRehenes * cantidadDeArmasDeLosLadrones unosLadrones

nivelAlgoPromedio :: (Rehen -> Int) ->[Rehen] -> Int
nivelAlgoPromedio complotOMiedo unosRehenes = (sum . map complotOMiedo $ unosRehenes) `div` length unosRehenes

cantidadDeArmasDeLosLadrones :: [Ladron] -> Int
cantidadDeArmasDeLosLadrones = sum . map (length . armas)


-- No entiendo este punto, que debe devolver la funcion, a los ladrones modificados o a los rehenes modificados? 
--  Que los rehenes se rebelen contra un ladrón, usando el plan que tengan en mente. Saben que es
-- mala idea, por lo que todos pierden 10 de complot antes de comenzar la rebelión.

-- losRehenesSeRebelan :: [Rehen] -> Ladron -> [Rehen]
-- losRehenesSeRebelan unosRehenes unLadron = map (flip rebelarse unLadron) unosRehenes

-- rebelarse :: Rehen -> Ladron -> Rehen
-- rebelarse unRehen unLadron = (head . planes $ unRehen) (modificarNivelComplot (-10) unRehen) unLadron