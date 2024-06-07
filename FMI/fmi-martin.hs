-------------------------------
----------- Punto 1 -----------
-------------------------------

data Pais = Pais{
    ingresoPerCapita :: Float,
    sectorPublico :: Float,
    sectorPrivado :: Float,
    recursos :: [Recurso],
    deudaFmi :: Float
}deriving (Show)

type Recurso = String

namibia :: Pais
namibia = Pais{
    ingresoPerCapita = 4140,
    sectorPublico = 400000,
    sectorPrivado = 650000,
    recursos = ["Mineria", "Ecoturismo"],
    deudaFmi = 50 -- Son millones
}

-------------------------------
----------- Punto 2 -----------
-------------------------------

-- Estas son estrategias, n estrategias componen una receta, después veo tipos

type Estrategia = Pais -> Pais

-- a)

prestarDolares :: Float -> Estrategia
prestarDolares valorPrestado unPais = agregarDeudaFmi (valorPrestado * 1.5) unPais

agregarDeudaFmi :: Float -> Pais -> Pais
agregarDeudaFmi valorIncremento unPais = modificarDeudaFmi (deudaFmi unPais + valorIncremento) unPais

modificarDeudaFmi :: Float -> Pais -> Pais
modificarDeudaFmi nuevaDeuda unPais = unPais {deudaFmi = nuevaDeuda}

-- b)

reducirPuestosPublicos :: Float -> Estrategia
reducirPuestosPublicos cantidadPuestos unPais
    | sectorPublico unPais > 100 = quitarPuestosPublicos cantidadPuestos . quitarPorcIngresoPerCapita 0.2 $ unPais
    | otherwise = quitarPuestosPublicos cantidadPuestos . quitarPorcIngresoPerCapita 0.15 $ unPais

quitarPuestosPublicos :: Float -> Pais -> Pais
quitarPuestosPublicos cantidadPuestosAQuitar unPais = modificarPuestosPublicos (sectorPublico unPais - cantidadPuestosAQuitar) unPais

modificarPuestosPublicos :: Float -> Pais -> Pais
modificarPuestosPublicos nuevosPuestos unPais = unPais{sectorPublico = nuevosPuestos}

quitarPorcIngresoPerCapita :: Float -> Pais -> Pais
quitarPorcIngresoPerCapita unPorcentaje unPais = modificarIngresoPerCapita (ingresoPerCapita unPais * (1 - unPorcentaje)) unPais

modificarIngresoPerCapita :: Float -> Pais -> Pais
modificarIngresoPerCapita nuevoIngreso unPais = unPais {ingresoPerCapita = nuevoIngreso}

-- c)

darRecursoAEmpresa :: Recurso -> Estrategia
darRecursoAEmpresa unRecurso unPais = quitarDeudaFmi 2 . quitarUnRecurso unRecurso $ unPais

quitarDeudaFmi :: Float -> Pais -> Pais
quitarDeudaFmi valorDecremento unPais = modificarDeudaFmi (deudaFmi unPais - valorDecremento) unPais

quitarUnRecurso :: Recurso -> Pais -> Pais
quitarUnRecurso unRecurso unPais = modificarRecursos (filter (/= unRecurso) (recursos unPais)) unPais

modificarRecursos :: [Recurso] -> Pais -> Pais
modificarRecursos nuevosRecursos unPais = unPais {recursos = nuevosRecursos}

-- d) 

establecerBlindaje :: Estrategia
establecerBlindaje unPais = reducirPuestosPublicos 500 . prestarDolares dolaresPrestados $ unPais
    where 
        dolaresPrestados = (ingresoPerCapita unPais * (sectorPublico unPais + sectorPrivado unPais)) / 1000000

-------------------------------
----------- Punto 3 -----------
-------------------------------

type Receta = [Estrategia]

receta :: Receta
receta = [prestarDolares 200000000, darRecursoAEmpresa "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unaReceta unPais = foldr aplicarEstrategia unPais unaReceta

aplicarEstrategia :: Estrategia -> Pais -> Pais
aplicarEstrategia unaEstrategia unPais = unaEstrategia unPais

-------------------------------
----------- Punto 4 -----------
-------------------------------

paisesQueZafan :: [Pais] -> [Pais]
paisesQueZafan listaPaises = filter (\x -> any (== "Petroleo") $ recursos x) listaPaises

totalDeudaFmi :: [Pais] -> Float
totalDeudaFmi listaPaises = sum . map deudaFmi $ listaPaises

-- En paisesQueZafan hay orden superior, al mandar por parámetro una expresión lambda más compleja para lo requerido, lo que hizo que sea mucho más sencillo poner una condición específica para encontrar lo que buscaba
-- En totalDeudaFmi uso tanto aplicación parcial como composición, lo que me ayuda a que sea mucho más claro el código y evitar la repetición de parámetros.

-------------------------------
----------- Punto 5 -----------
-------------------------------

recetasDePeorAMejor :: Pais -> [Receta] -> Bool
recetasDePeorAMejor _ [] = True
recetasDePeorAMejor _ [unaReceta] = True
recetasDePeorAMejor unPais (unaReceta : otraReceta : masRecetas) = obtenerPBI (aplicarReceta unaReceta unPais) < obtenerPBI (aplicarReceta otraReceta unPais) && recetasDePeorAMejor unPais (otraReceta : masRecetas)


obtenerPBI :: Pais -> Float
obtenerPBI unPais = ingresoPerCapita unPais * (sectorPublico unPais + sectorPrivado unPais)

-------------------------------
----------- Punto 6 -----------
-------------------------------

recursosNaturalesInfinitos :: [Recurso]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

namibia2 :: Pais
namibia2 = Pais{
    ingresoPerCapita = 4140,
    sectorPublico = 400000,
    sectorPrivado = 650000,
    recursos = recursosNaturalesInfinitos,
    deudaFmi = 50 -- Son millones
}

-- En el caso del punto 4a, nunca se va a poder llegar a un resultado en este caso, ya que, por más que se aplique lazy evaluation, al ser "Energia" el unico recurso que tiene el pais repetido indefinidamente, entonces se sigue generando la lista infinita constantemente a medida que se sigue buscando el recurso "Petroleo". Es por esto que se queda tildado intentando buscar.

-- En el caso del punto 4b, sí se va a poder llegar al resultado, porque nunca se utiliza la lista de recursos, por lo que, siguiendo el concepto de lazy evaluation, nunca existirá como tal la lista infinita y se podrá trabajar sobre los demás campos con tranquilidad.
