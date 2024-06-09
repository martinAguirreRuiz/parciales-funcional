import Data.List (genericLength, find)
import Text.Show.Functions

-----------------------------------
------------- Punto 1 -------------
-----------------------------------

data Pais = Pais {
    ingresoPerCapita :: Float,
    sectorPublico :: Float,
    sectorPrivado :: Float,
    recursosNaturales :: [RecursoNatural],
    deudaEnMillones :: Float
} deriving (Show)

type RecursoNatural = String

type Estrategia = Pais -> Pais

namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

-----------------------------------
------------- Punto 2 -------------
-----------------------------------

prestarMillones :: Float -> Estrategia
prestarMillones unosMillones unPais = unPais {deudaEnMillones = deudaEnMillones unPais + unosMillones * 1.5}

reducirTrabajoPublico :: Float -> Estrategia
reducirTrabajoPublico cantAReducir unPais = unPais {sectorPublico = sectorPublico unPais - cantAReducir, ingresoPerCapita = ingresoPerCapita unPais * reduccionIngreso unPais}


reduccionIngreso :: Pais -> Float
reduccionIngreso unPais
    | sectorPrivado unPais > 100 = 0.8
    | otherwise = 0.85

darExplotacion :: RecursoNatural -> Estrategia
darExplotacion unRecurso unPais = quitarRecurso unRecurso unPais {deudaEnMillones = deudaEnMillones unPais - 2}

quitarRecurso :: RecursoNatural -> Estrategia
quitarRecurso unRecurso unPais = unPais {recursosNaturales = filter (/= unRecurso) (recursosNaturales unPais)}


establecerBlindaje :: Estrategia
establecerBlindaje unPais = prestarMillones (pbi unPais * 0.5) . reducirTrabajoPublico 500 $ unPais

pbi :: Pais -> Float
pbi unPais = ingresoPerCapita unPais * (sectorPublico unPais + sectorPrivado unPais)

-----------------------------------
------------- Punto 3 -------------
-----------------------------------

type Receta = [Estrategia]

receta :: Receta
receta = [prestarMillones 200, darExplotacion "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unaReceta unPais = foldl (\unPais unaEstrategia -> unaEstrategia unPais) unPais unaReceta

--El efecto colateral se logra dando un nuevo pais llamado namibia modificado que tiene una deuda de 200 millones mas que el original y no tiene el recurso natural "Mineria", esto se debe a que en haskell no se pueden modificar los valores de un pais ya que son inmutables, por lo que se debe crear un nuevo pais con los valores modificados.

-----------------------------------
------------- Punto 4 -------------
-----------------------------------

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo" . recursosNaturales

totalDeuda :: [Pais] -> Float
totalDeuda = sum . map deudaEnMillones

-- composicion: se compone elem "petroleo" con recursosNaturales para saber si el pais tiene petroleo como un recurso Natural. 
-- Aplicacion parcial: se aplica la funcion elem parcialmente, en vez de pasarle sus dos argumentos solo le pasamos el primero.
-- Orden superior: se le pasa una funcion a filter para que filtre los paises que cumplan con la condicion de tener petroleo como recurso natural.

subioElPBI :: Pais -> Pais -> Bool
subioElPBI unPais elPaisModificado = pbi unPais < pbi elPaisModificado 

estaOrdenadaDePeorAMejor :: [Estrategia] -> Pais -> Bool
estaOrdenadaDePeorAMejor [] _ = True
estaOrdenadaDePeorAMejor [estrategia] _ = True
estaOrdenadaDePeorAMejor (estrategia:estrategias) unPais = subioElPBI unPais (estrategia unPais) && estaOrdenadaDePeorAMejor estrategias (estrategia unPais)

