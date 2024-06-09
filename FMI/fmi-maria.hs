import Data.List (genericLength, find)
import Text.Show.Functions
import qualified Control.Applicative as primero

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
reducirTrabajoPublico cantAReducir unPais
    | sectorPublico unPais > 100 = modificarTrabajoPublico (sectorPublico unPais - cantAReducir) . modificarIngresoPerCapita (ingresoPerCapita unPais * 0.8) $ unPais 
    | otherwise = modificarTrabajoPublico (sectorPublico unPais - cantAReducir) . modificarIngresoPerCapita (ingresoPerCapita unPais * 0.85) $ unPais 

modificarTrabajoPublico :: Float -> Pais -> Pais
modificarTrabajoPublico nuevoValor unPais = unPais {sectorPublico =  max 0 nuevoValor}

modificarIngresoPerCapita :: Float -> Pais -> Pais
modificarIngresoPerCapita nuevoValor unPais = unPais {ingresoPerCapita =  max 0 nuevoValor}

darExplotacion :: RecursoNatural -> Estrategia
darExplotacion unRecurso unPais = modificarRecursosNaturales (filter (/= unRecurso) (recursosNaturales unPais)) . modificarDeuda (deudaEnMillones unPais - 2) $ unPais

modificarRecursosNaturales :: [RecursoNatural] -> Pais -> Pais
modificarRecursosNaturales nuevosRecursos unPais = unPais {recursosNaturales = nuevosRecursos}

modificarDeuda :: Float -> Pais -> Pais
modificarDeuda nuevaDeuda unPais = unPais {deudaEnMillones = nuevaDeuda}

establecerBlindaje :: Estrategia
establecerBlindaje unPais = modificarDeuda (deudaEnMillones unPais + 0.5 * pbi) . modificarTrabajoPublico (sectorPrivado unPais - 500) $ unPais

pbi :: Pais -> Float
pbi = ingresoPerCapita unPais * (sectorPublico unPais + sectorPrivado unPais)

-----------------------------------
------------- Punto 3 -------------
-----------------------------------

prestar200MillonesydarMineria :: Estrategia
prestar200MillonesydarMineria = prestarMillones 200 . darExplotacion "Mineria"

namibiaModificado :: Pais
namibiaModificado = prestar200MillonesydarMineria namibia

--El efecto colateral se logra dando un nuevo pais llamado namibia modificado que tiene una deuda de 200 millones mas que el original y no tiene el recurso natural "Mineria", esto se debe a que en haskell no se pueden modificar los valores de un pais ya que son inmutables, por lo que se debe crear un nuevo pais con los valores modificados.

-----------------------------------
------------- Punto 4 -------------
-----------------------------------

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (elem "Petroleo" . recursosNaturales)

totalDeuda :: [Pais] -> Float
totalDeuda = sum . map deudaEnMillones

-- composicion: se compone elem "petroleo" con recursosNaturales para saber si el pais tiene petroleo como un recurso Natural. 
-- Aplicacion parcial: se aplica la funcion elem parcialmente, en vez de pasarle sus dos argumentos solo le pasamos el primero.
-- Orden superior: se le pasa una funcion a filter para que filtre los paises que cumplan con la condicion de tener petroleo como recurso natural.

subioElPBI :: Pais -> Pais -> Bool
subioElPBI unPais elPaisModificado = pbi unPais < pbi elPaisModificado 

estaOrdenadaDePeorAMejor :: [Estrategia] -> Pais -> Bool
estaOrdenadaDePeorAMejor [estrategia] _ = True
estaOrdenadaDePeorAMejor estrategia:estrategias unPais = subioElPBI unPais (aplicarEstrategia unPais) && estaOrdenadaDePeorAMejor estrategias (aplicarEstrategia unPais)

aplicarEstrategia :: Estrategia -> Pais -> Pais
aplicarEstrategia unasEstrategia unPais = unaEstrategia unPais