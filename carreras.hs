import Data.List (genericLength, find)
import Text.Show.Functions

-----------------------------
---------- Punto 1 ----------
-----------------------------

data Auto = Auto {
    color :: String,
    velocidad :: Float,
    distancia :: Float
}

data Carrera = Carrera{
    estadoAutos :: [Auto]
}

-- a)

autoEstaCercaDeOtro :: Auto -> Auto -> Bool
autoEstaCercaDeOtro unAuto otroAuto = (autosSonDistintos unAuto otroAuto) && (distanciaEntreAutosMenorA 10 unAuto otroAuto)

autosSonDistintos :: Auto -> Auto -> Bool
autosSonDistintos unAuto otroAuto = color unAuto /= color otroAuto

distanciaEntreAutosMenorA :: Float -> Auto -> Auto -> Bool
distanciaEntreAutosMenorA unaDistancia unAuto otroAuto = abs (distancia unAuto - distancia otroAuto) < 10

-- b)

autoVaTranquilo :: Auto -> Carrera -> Bool
autoVaTranquilo unAuto unaCarrera = not (autoEstaCercaDeOtros unAuto listaAutos) && autoVaGanando unAuto listaAutos
    where
        listaAutos = estadoAutos unaCarrera

autoEstaCercaDeOtros :: Auto -> [Auto] -> Bool
autoEstaCercaDeOtros _ [] = True
autoEstaCercaDeOtros unAuto (otroAuto : masAutos) = autoEstaCercaDeOtro unAuto otroAuto && autoEstaCercaDeOtros unAuto masAutos

autoVaGanando :: Auto -> [Auto] -> Bool
autoVaGanando unAuto listaAutos = all (\x -> distancia x < distancia unAuto) listaAutos

autoVaGanandoAOtro :: Auto -> Auto -> Bool
autoVaGanandoAOtro unAuto otroAuto = distancia unAuto >= distancia otroAuto

-- c)

puestoDeUnAutoEnCarrera :: Auto -> Carrera -> Int
puestoDeUnAutoEnCarrera unAuto unaCarrera = cantidadDeAutosQueGananAUnAuto unAuto listaAutos + 1
    where 
        listaAutos = estadoAutos unaCarrera


cantidadDeAutosQueGananAUnAuto :: Auto -> [Auto] -> Int
cantidadDeAutosQueGananAUnAuto unAuto listaAutos = length (filter (not . autoVaGanandoAOtro unAuto) listaAutos)

-----------------------------
---------- Punto 2 ----------
-----------------------------

-- a)

autoCorreDuranteUnTiempo :: Float -> Auto -> Auto
autoCorreDuranteUnTiempo unTiempo unAuto = modificarDistanciaDeUnAuto (distanciaActual + velocidadActual * unTiempo) unAuto
    where 
        distanciaActual = distancia unAuto
        velocidadActual = velocidad unAuto

modificarDistanciaDeUnAuto :: Float -> Auto -> Auto
modificarDistanciaDeUnAuto distanciaNueva unAuto = unAuto {distancia = distanciaNueva}

-- b.1)

type Modificador = Float -> Float

alterarVelocidadDeUnAuto :: Modificador -> Auto -> Auto
alterarVelocidadDeUnAuto unModificador unAuto = unAuto {velocidad = unModificador(velocidad unAuto)}
 
-- b.2)

bajarVelocidadDeUnAuto :: Float -> Auto -> Auto
bajarVelocidadDeUnAuto cantidadABajar unAuto 
    | velocidad unAutoCambiado > 0 = unAutoCambiado
    | otherwise = unAuto {velocidad = 0}
    where
        unAutoCambiado = alterarVelocidadDeUnAuto modificadorBajarVelocidad unAuto

modificadorBajarVelocidad :: Modificador
modificadorBajarVelocidad cantidadABajar = - cantidadABajar

-----------------------------
---------- Punto 3 ----------
-----------------------------

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto unAuto unaCarrera = modificarCarrera autosAfectados unaCarrera
    where 
        listaAutos = estadoAutos unaCarrera
        autosAfectados = afectarALosQueCumplen (autoEstaCercaDeOtro unAuto) (bajarVelocidadDeUnAuto 50) listaAutos

modificarCarrera :: [Auto] -> Carrera -> Carrera 
modificarCarrera nuevaListaAutos unaCarrera = unaCarrera {estadoAutos = nuevaListaAutos}

miguelitos :: Float -> PowerUp
miguelitos cantidadABajar unAuto unaCarrera = modificarCarrera autosAfectados unaCarrera
    where 
        listaAutos = estadoAutos unaCarrera
        autosAfectados = afectarALosQueCumplen (autoVaGanandoAOtro unAuto) (bajarVelocidadDeUnAuto cantidadABajar)listaAutos

jetPack :: Float -> PowerUp
jetPack unTiempo unAuto unaCarrera = modificarCarrera listaAutosNueva unaCarrera
    where 
        listaAutos = estadoAutos unaCarrera
        autoDespuesDeCorrer = autoVuelveAVelocidadOriginal . autoCorreDuranteUnTiempo unTiempo . autoConDobleVelocidad $ unAuto 
        listaAutosNueva = autosQueVanGanando unAuto listaAutos ++ [unAuto] ++ autosQueVanPerdiendo unAuto listaAutos

autosQueVanGanando :: Auto -> [Auto] -> [Auto]
autosQueVanGanando unAuto listaAutos = drop (cantidadDeAutosQueGananAUnAuto unAuto listaAutos + 1) listaAutos 

autosQueVanPerdiendo :: Auto -> [Auto] -> [Auto]
autosQueVanPerdiendo unAuto listaAutos = filter (not . autoVaGanandoAOtro unAuto) listaAutos

modificadorDuplicarVelocidad :: Modificador
modificadorDuplicarVelocidad unaVelocidad = unaVelocidad * 2

autoConDobleVelocidad :: Auto -> Auto
autoConDobleVelocidad unAuto = alterarVelocidadDeUnAuto modificadorDuplicarVelocidad unAuto

modificadorDividirVelocidad :: Modificador
modificadorDividirVelocidad unaVelocidad = unaVelocidad / 2

autoVuelveAVelocidadOriginal :: Auto -> Auto
autoVuelveAVelocidadOriginal unAuto = alterarVelocidadDeUnAuto modificadorDividirVelocidad unAuto

-----------------------------
---------- Punto 4 ----------
-----------------------------

-- 4.a) 

type Color = String
type Evento = Carrera -> Carrera

simularCarrera :: Carrera -> [Evento] -> [(Int, String)]
simularCarrera unaCarrera listaEventos = zip [1..length(listaAutosNueva)] [color (head listaAutosNueva)..color(last listaAutosNueva)]
    where
        listaAutos = estadoAutos unaCarrera
        listaAutosNueva = estadoAutos (foldr (aplicarEvento) (unaCarrera) listaEventos)

aplicarEvento :: Evento -> Carrera -> Carrera
aplicarEvento unEvento unaCarrera = unEvento unaCarrera

-- foldr (+) 10 [1,2,3] == 16



-- 4.b)

correnTodos :: Float -> Carrera -> Carrera
correnTodos unTiempo unaCarrera = modificarCarrera listaAutosNueva unaCarrera
    where 
        listaAutos = estadoAutos unaCarrera
        listaAutosNueva = map (autoCorreDuranteUnTiempo unTiempo) listaAutos

-- usaPowerUp :: Auto -> PowerUp -> Carrera -> Auto
-- usaPowerUp unAuto unPowerUp unaCarrera = 
--     where 
--         listaAutos = estadoAutos unaCarrera





