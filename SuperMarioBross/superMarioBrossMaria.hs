-- Alumna = {
--     nombre = "Maria",
--     apellido = "Musante",
--     legajo = 211.795-2
-- }

import Data.List (genericLength, find)
import Text.Show.Functions

data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Float
}deriving (Show)

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Float,
    material :: String
}deriving (Show , Eq)




---------------------------------------
--------------- Punto 1 ---------------
---------------------------------------


mario :: Plomero
mario = Plomero {
    nombre = "Mario",
    cajaDeHerramientas = [llaveInglesa, martillo],
    historialReparaciones = [],
    dinero = 1200
}

llaveInglesa :: Herramienta
llaveInglesa = Herramienta {
    denominacion = "Llave Inglesa",
    precio = 200,
    material = "hierro"
}

martillo :: Herramienta
martillo = Herramienta {
    denominacion = "Martillo",
    precio = 20,
    material = "madera"
}

wario :: Plomero
wario = Plomero {
    nombre = "Wario",
    cajaDeHerramientas = iterate generarNuevaLlaveFrancesa llaveFrancesa,
    historialReparaciones = [],
    dinero = 0.5
}

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta {
    denominacion = "Llave Francesa",
    precio = 1,
    material = "hierro"
}

generarNuevaLlaveFrancesa :: Herramienta -> Herramienta
generarNuevaLlaveFrancesa laLlave = laLlave { precio = precio laLlave + 1}

---------------------------------------
--------------- Punto 2 ---------------
---------------------------------------

tieneUnaHerramienta :: Herramienta -> Plomero -> Bool
tieneUnaHerramienta unaHerramienta = elem unaHerramienta . cajaDeHerramientas

esMalvado :: Plomero -> Bool
esMalvado = empiezaConWa . nombre

empiezaConWa :: String -> Bool
empiezaConWa = (== "Wa") . take 2

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta = (>= precio unaHerramienta) . dinero

---------------------------------------
--------------- Punto 3 ---------------
---------------------------------------

esBuena :: Herramienta -> Bool
esBuena unaHerramienta = esDeHierroYCara unaHerramienta || esMartilloDeMaderaOGoma unaHerramienta

esDeHierroYCara :: Herramienta -> Bool
esDeHierroYCara unaHerramienta = material unaHerramienta == "hierro" && precio unaHerramienta > 10000

esMartilloDeMaderaOGoma :: Herramienta -> Bool
esMartilloDeMaderaOGoma unaHerramienta = denominacion unaHerramienta == "Martillo" && (material unaHerramienta == "madera" || material unaHerramienta == "goma")

---------------------------------------
--------------- Punto 4 ---------------
---------------------------------------

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    | puedeComprar unaHerramienta unPlomero = agregarALaCaja unaHerramienta . usarBilletera (- precio unaHerramienta) $ unPlomero
    | otherwise = unPlomero

agregarALaCaja :: Herramienta -> Plomero -> Plomero
agregarALaCaja unaHerramienta unPlomero = unPlomero { cajaDeHerramientas = unaHerramienta : cajaDeHerramientas unPlomero }

usarBilletera :: Float -> Plomero -> Plomero
usarBilletera unaCantidad unPlomero = unPlomero { dinero = dinero unPlomero + unaCantidad }

---------------------------------------
--------------- Punto 5 ---------------
---------------------------------------

data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Requerimiento
}deriving (Show)

type Requerimiento = Plomero -> Bool

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion {
    descripcion = "Filtracion de agua",
    requerimiento = tieneUnaHerramienta llaveInglesa
}

esDificil :: Reparacion -> Bool
esDificil unaReparacion = esUnGrito (descripcion unaReparacion) && esComplicada (descripcion unaReparacion)

esUnGrito :: String -> Bool
esUnGrito = all esMayuscula

esComplicada :: String -> Bool
esComplicada  = (>= 100) . genericLength 

esMayuscula :: Char -> Bool
esMayuscula unaLetra = elem unaLetra " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

presupuesto :: Reparacion -> Float
presupuesto = (* 3) . genericLength . descripcion 


---------------------------------------
--------------- Punto 6 ---------------
---------------------------------------

visitarParaReparacion :: Reparacion -> Plomero -> Plomero
visitarParaReparacion unaReparacion unPlomero
    | cumpleRequerimiento unaReparacion unPlomero || esMalvado unPlomero && tieneUnaHerramienta martillo unPlomero = hacerReparacion unaReparacion unPlomero
    | otherwise = usarBilletera 100 unPlomero

cumpleRequerimiento :: Reparacion -> Plomero -> Bool
cumpleRequerimiento = requerimiento 

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion = olvidarORobarHerramientas unaReparacion .  agregarReparacion unaReparacion . usarBilletera (presupuesto unaReparacion) 

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = unPlomero { historialReparaciones = unaReparacion : historialReparaciones unPlomero }

olvidarORobarHerramientas :: Reparacion -> Plomero -> Plomero
olvidarORobarHerramientas unaReparacion unPlomero
    | esMalvado unPlomero = agregarALaCaja destornilladorRobado unPlomero
    | esDificil unaReparacion = unPlomero {cajaDeHerramientas = filter (not . esBuena) (cajaDeHerramientas unPlomero)}
    | otherwise = unPlomero {cajaDeHerramientas = tail . cajaDeHerramientas $ unPlomero}

destornilladorRobado :: Herramienta
destornilladorRobado = Herramienta {
    denominacion = "Destornillador",
    precio = 0,
    material = "plastico"
}

---------------------------------------
--------------- Punto 7 ---------------
---------------------------------------
type JornadaDeTrabajo = [Reparacion]

sufrirJornadaDeTrabajo :: Plomero -> JornadaDeTrabajo -> Plomero
sufrirJornadaDeTrabajo = foldr visitarParaReparacion

---------------------------------------
--------------- Punto 8 ---------------
---------------------------------------

-- elMasReparador :: [Plomero] -> JornadaDeTrabajo -> Plomero
-- elMasReparador unosPlomeros unaJornada = elQueMasTrabajo . queSufranLaJornadaTodos unosPlomeros $ unaJornada

-- queSufranLaJornadaTodos :: [Plomero] -> JornadaDeTrabajo -> [Plomero]
-- queSufranLaJornadaTodos unosPlomeros unaJornada = map (flip sufrirJornadaDeTrabajo unaJornada) unosPlomeros

-- elQueMasTrabajo :: [Plomero] -> Plomero
-- elQueMasTrabajo (unPlomero:otroPlomero:masPlomeros) = compararDeADos unPlomero otroPlomero


elMasReparador :: [Plomero] -> JornadaDeTrabajo -> Plomero
elMasReparador unosPlomeros = compararSegunCriterio cantidadDeReparaciones . queSufranLaJornadaTodos unosPlomeros  
    where cantidadDeReparaciones = genericLength.historialReparaciones

elMasAdinerado :: [Plomero] -> JornadaDeTrabajo -> Plomero
elMasAdinerado unosPlomeros  = compararSegunCriterio dinero . queSufranLaJornadaTodos unosPlomeros


elQueMasInvirtio :: [Plomero] -> JornadaDeTrabajo -> Plomero
elQueMasInvirtio unosPlomeros = compararSegunCriterio dineroInvertido . queSufranLaJornadaTodos unosPlomeros
    where dineroInvertido = sum . map precio . cajaDeHerramientas
            

queSufranLaJornadaTodos :: [Plomero] -> JornadaDeTrabajo -> [Plomero]
queSufranLaJornadaTodos unosPlomeros unaJornada = map (flip sufrirJornadaDeTrabajo unaJornada) unosPlomeros

compararSegunCriterio :: (Plomero -> Float) -> [Plomero] -> Plomero
compararSegunCriterio unCriterio (unPlomero:masPlomeros) = foldr (cualEsMejor unCriterio) unPlomero masPlomeros


cualEsMejor :: (Plomero -> Float) -> Plomero -> Plomero -> Plomero
cualEsMejor unCriterio unPlomero otroPlomero
    | unCriterio unPlomero > unCriterio otroPlomero = unPlomero
    | otherwise = otroPlomero




