import Data.List (genericLength, find)
import Text.Show.Functions

-------------------
----- PUNTO 1 -----
-------------------

data Plomero = UnPlomero{
    nombre :: String,
    cajaHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Float
}deriving (Show)

data Herramienta = UnaHerramienta{
    denominacion :: String,
    precio :: Float,
    materialEmpuniadura :: String
}deriving (Show, Eq)

-- a)

mario :: Plomero
mario = UnPlomero{
    nombre = "Mario",
    dinero = 1200,
    historialReparaciones = [],
    cajaHerramientas = [UnaHerramienta "Llave Inglesa" 200 "Hierro", UnaHerramienta "Martillo" 20 "Madera"]
}

-- b)

wario :: Plomero
wario = UnPlomero{
    nombre = "Wario",
    dinero = 0.5,
    historialReparaciones = [],
    cajaHerramientas = iterate sumarPrecioAHerramienta $ UnaHerramienta "Llave Francesa" 1 "Hierro"
}

sumarPrecioAHerramienta :: Herramienta -> Herramienta
sumarPrecioAHerramienta unaHerramienta = unaHerramienta {precio = precio unaHerramienta + 1}

-------------------
----- PUNTO 2 -----
-------------------

-- a)

tieneHerramientaConDenominacion :: String -> Plomero -> Bool
tieneHerramientaConDenominacion unaDenominacion = any (== unaDenominacion) . map denominacion . cajaHerramientas

-- b)

esMalvado :: Plomero -> Bool
esMalvado unPlomero = (== "Wa") . take 2 . nombre $ unPlomero

-- c)

puedeComprarHerramienta :: Herramienta -> Plomero -> Bool
puedeComprarHerramienta unaHerramienta unPlomero = dinero unPlomero >= precio unaHerramienta

-------------------
----- PUNTO 3 -----
-------------------

esBuena :: Herramienta -> Bool
esBuena unaHerramienta = martilloEspecial unaHerramienta || empuniaduraEspecial unaHerramienta

empuniaduraEspecial :: Herramienta -> Bool
empuniaduraEspecial unaHerramienta = material == "Hierro" && costo > 10000
    where 
        material = materialEmpuniadura unaHerramienta
        costo = precio unaHerramienta

martilloEspecial :: Herramienta -> Bool
martilloEspecial unaHerramienta = herramienta == "Martillo" && (material == "Madera" || material == "Goma")
    where 
        material = materialEmpuniadura unaHerramienta
        herramienta = denominacion unaHerramienta

-------------------
----- PUNTO 4 -----
-------------------

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    | puedeComprarHerramienta unaHerramienta unPlomero = sumarHerramienta unaHerramienta . gastarDinero (precio unaHerramienta) $ unPlomero
    | otherwise = unPlomero

gastarDinero :: Float -> Plomero -> Plomero
gastarDinero valorAGastar unPlomero = unPlomero {dinero = dinero unPlomero - valorAGastar}

sumarHerramienta :: Herramienta -> Plomero -> Plomero
sumarHerramienta unaHerramienta unPlomero = unPlomero {cajaHerramientas = unaHerramienta : cajaHerramientas unPlomero}

-------------------
----- PUNTO 5 -----
-------------------

-- a)

data Reparacion = UnaReparacion{
    descripcion :: String,
    requerimiento :: Requerimiento
}deriving (Show)

type Requerimiento = Plomero -> Bool

filtracionAgua :: Reparacion
filtracionAgua = UnaReparacion{
    descripcion = "Se pierde agua",
    requerimiento = tieneLlaveInglesa
}

tieneLlaveInglesa :: Requerimiento
tieneLlaveInglesa = any (== "Llave Inglesa") . map denominacion . cajaHerramientas 

-- b)

esDificil :: Reparacion -> Bool
esDificil unaReparacion = descripcionComplicada (descripcion unaReparacion)

descripcionComplicada :: String -> Bool
descripcionComplicada unaDescripcion = esUnGrito unaDescripcion && ((> 100) . length $ unaDescripcion)

esUnGrito :: String -> Bool
esUnGrito unaDescripcion = all (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ ") unaDescripcion

-- c)

presupuesto :: Reparacion -> Float
presupuesto unaReparacion = (* 3) . genericLength . descripcion $ unaReparacion

-------------------
----- PUNTO 6 -----
-------------------

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion unPlomero
    | puedeHacerYEsMalo                  = sumarHerramienta (UnaHerramienta "Destornillador" 0 "Plastico") . realizar unaReparacion $ unPlomero
    | puedeHacerYEsDificil               = quitarTodasLasHerramientas . realizar unaReparacion $ unPlomero
    | puedeHacer unaReparacion unPlomero = pierdeHerramienta . realizar unaReparacion $ unPlomero
    | otherwise                          = sumarDinero 100 unPlomero
    where
        puedeHacerYEsMalo = puedeHacer unaReparacion unPlomero && esMalvado unPlomero
        puedeHacerYEsDificil = puedeHacer unaReparacion unPlomero && esDificil unaReparacion


puedeHacer :: Reparacion -> Plomero -> Bool
puedeHacer unaReparacion unPlomero = requerimiento unaReparacion unPlomero || (tieneHerramientaConDenominacion "Martillo" unPlomero && esMalvado unPlomero)

realizar :: Reparacion -> Plomero -> Plomero
realizar unaReparacion unPlomero = agregarReparacion unaReparacion . sumarDinero (presupuesto unaReparacion) $ unPlomero

sumarDinero :: Float -> Plomero -> Plomero
sumarDinero dineroASumar unPlomero = unPlomero {dinero = dinero unPlomero + dineroASumar}

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion nuevaReparacion unPlomero = unPlomero {historialReparaciones = nuevaReparacion : historialReparaciones unPlomero}

pierdeHerramienta :: Plomero -> Plomero 
pierdeHerramienta unPlomero = unPlomero {cajaHerramientas = tail . cajaHerramientas $ unPlomero}

quitarTodasLasHerramientas :: Plomero -> Plomero
quitarTodasLasHerramientas unPlomero = unPlomero {cajaHerramientas = []}

-------------------
----- PUNTO 7 -----
-------------------

jornadaDeTrabajo :: [Reparacion] -> Plomero -> Plomero
jornadaDeTrabajo listaReparaciones unPlomero = foldr hacerReparacion unPlomero listaReparaciones


-------------------
----- PUNTO 8 -----
-------------------

-- a)

masReparador :: [Plomero] -> Plomero
masReparador listaPlomeros = tomarMayor maximasReparaciones listaPlomeros

maximasReparaciones :: [Plomero] -> Float
maximasReparaciones listaPlomeros = maximum . map genericLength . map historialReparaciones $ listaPlomeros

tomarMayor :: ([Plomero] -> Float) -> [Plomero] -> Plomero
tomarMayor f listaPlomeros = head . takeWhile (\unPlomero -> dinero unPlomero == f listaPlomeros) $ listaPlomeros

-- b)

masAdinerado :: [Plomero] -> Plomero
masAdinerado listaPlomeros = tomarMayor maximoDinero listaPlomeros

maximoDinero :: [Plomero] -> Float
maximoDinero listaPlomeros = maximum . map dinero $ listaPlomeros

-- c)

masInversor :: [Plomero] -> Plomero
masInversor listaPlomeros = tomarMayor masDineroEnHerramientas listaPlomeros

masDineroEnHerramientas :: [Plomero] -> Float
masDineroEnHerramientas listaPlomeros = maximum . map sum . map (map precio) . map cajaHerramientas $ listaPlomeros
