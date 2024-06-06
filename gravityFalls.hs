import Data.List

--------------------------------  PRIMERA PARTE --------------------------------

---------------------------
--------- Punto 1 ---------
---------------------------

data Persona = Persona {
    edad :: Int,
    items :: [Item],
    experiencia :: Float
}

type Item = String

dipper :: Persona
dipper = Persona{
    edad = 15,
    items = ["Soplador de Hojas"],
    experiencia = 30
}

data Criatura = Criatura {
    peligrosidad :: Float,
    condicionQueDestruye :: Condicion
}

type Condicion = Persona -> Bool

siempreDetras :: Criatura
siempreDetras = Criatura {
    peligrosidad = 0,
    condicionQueDestruye = condicionSiempreDetras
}

condicionSiempreDetras :: Persona -> Bool
condicionSiempreDetras _ = False

grupo10Gnomos :: Criatura
grupo10Gnomos = Criatura {
    peligrosidad = 2 ^ 10,
    condicionQueDestruye = condicionGrupo10Gnomos
}

condicionGrupo10Gnomos :: Persona -> Bool
condicionGrupo10Gnomos = elem "Soplador de Hojas" . items 

fantasmaCategoria3 :: Criatura
fantasmaCategoria3 = Criatura {
    peligrosidad = 20 * 3,
    condicionQueDestruye = condicionFantasmaCategoria3 
}

condicionFantasmaCategoria3 :: Persona -> Bool
condicionFantasmaCategoria3 unaPersona = elem "Disfraz de Oveja" (items unaPersona) && edad unaPersona < 13

fantasmaCategoria1 :: Criatura
fantasmaCategoria1 = Criatura {
    peligrosidad = 20 * 3,
    condicionQueDestruye = condicionFantasmaCategoria1
}

condicionFantasmaCategoria1 :: Persona -> Bool
condicionFantasmaCategoria1 = (> 10) . experiencia

---------------------------
--------- Punto 2 ---------
---------------------------

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura unaPersona unaCriatura 
    | (condicionQueDestruye unaCriatura) unaPersona == True = ganarExperiencia (peligrosidad unaCriatura) unaPersona
    | otherwise = ganarExperiencia (1) unaPersona


ganarExperiencia :: Float -> Persona -> Persona
ganarExperiencia cantidadGanada unaPersona = unaPersona {experiencia = experiencia unaPersona + cantidadGanada}

---------------------------
--------- Punto 3 ---------
---------------------------

experienciaGanada :: Persona -> [Criatura] -> Float
experienciaGanada unaPersona listaCriaturas = experiencia (personaDespuesEnfrentar unaPersona listaCriaturas) - experiencia unaPersona

personaDespuesEnfrentar :: Persona -> [Criatura] -> Persona
personaDespuesEnfrentar unaPersona listaCriaturas = foldr (flip enfrentarCriatura) unaPersona listaCriaturas


--------------------------------  SEGUNDA PARTE --------------------------------

---------------------------
--------- Punto 1 ---------
---------------------------

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
-- zipWithIf :: (operacion) -> (condicion) -> (lista) -> (lista) -> (lista)
zipWithIf _ _ _ [] = []
zipWithIf operacion condicion (x:xs) (y:ys)
    | condicion y == True = (operacion x y) : zipWithIf operacion condicion (xs) ys
    | otherwise = y : zipWithIf operacion condicion (x:xs) ys

---------------------------
--------- Punto 2 ---------
---------------------------

abecedarioDesde :: Char -> [Char]
abecedarioDesde unaLetra = [unaLetra..'z'] ++ (init ['a'..unaLetra])

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave = encontrarLetraFinal . posicionLetraAEncontrar letraClave

encontrarLetraFinal :: Int -> Char
encontrarLetraFinal indice = (!!) ['a'..'z'] indice

posicionLetraAEncontrar :: Char -> Char -> Int
posicionLetraAEncontrar letraClave letraAEncontrar = length (abecedarioHasta letraAEncontrar letraClave)

abecedarioHasta :: Char -> Char -> [Char]
abecedarioHasta letraAEncontrar letraClave = (takeWhile (/= letraAEncontrar) (abecedarioDesde letraClave))

cesar :: Char -> String -> String
cesar letraClave textoEncriptado = zipWithIf desencriptarLetra (esLetra) (repeat letraClave) textoEncriptado

esLetra :: Char -> Bool
esLetra unaLetra = elem unaLetra ['a'..'z']

--  map (`cesar` "jrzel zrfaxal!") ['a'..'z']

---------------------------
--------- Punto 3 ---------
---------------------------

-- vigenere :: String -> String -> String
-- vigenere textoClave textoEncriptado = zipWithIf (desencriptarLetra) (esLetra) (alinearTextoClave textoClave textoEncriptado)

-- alinearTextoClave :: String -> String -> String
-- alinearTextoClave textoClave textoEncriptado = zipWithIf (reemplazarCaracter) (esLetra) (cycle textoClave) textoEncriptado

-- reemplazarCaracter :: Char -> Char -> Char
-- reemplazarCaracter charNuevo charViejo = charNuevo

-- desencriptarTexto :: String -> String -> String
-- desencriptarTexto _ [] = []
-- desencriptarTexto (x:xs) textoEncriptado = (!!) 0 (cesar x (take 1 textoEncriptado)) : desencriptarTexto xs (drop 1 textoEncriptado)

-- desencriptarTexto textoClave textoEncriptado = zipWithIf (cesar') (esLetra) (alinearTextoClave textoClave textoEncriptado) textoEncriptado
