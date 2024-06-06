import Text.Show.Functions

---------------------------
-------- Punto 1 ----------
data Personaje = UnPersonaje
  { nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    planeta :: Planeta
  }
  deriving (Show)

type Habilidad = String

type Planeta = String

type Universo = [Personaje]

data Guantelete = UnGuantelete
  { material :: Material,
    gemas :: [Gema]
  }
  deriving (Show)

data Material = Hierro | Uru | Goma deriving (Show, Eq)

type Gema = Personaje -> Personaje

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universoOriginal
  | guanteleteCompleto = take mitad universoOriginal
  | otherwise = universoOriginal
  where
    mitad = length universoOriginal `div` 2
    guanteleteCompleto = ((== 6) . length . gemas) guantelete && ((== Uru) . material) guantelete

---------------------------
-------- Punto 2 ----------
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.

aptoPendex :: Universo -> Bool
aptoPendex = any ((< 45) . edad)

energiaTotal :: Universo -> Int
energiaTotal universo = sum (map energia listaFiltrada)
  where
    listaFiltrada = filter ((> 1) . length . habilidades) universo

---------------------------
-------- Punto 3 ----------
mente :: Int -> Gema
mente valor = disminuirEnergia valor

alma :: Habilidad -> Personaje -> Personaje
alma habilidad personaje = eliminarHabilidad habilidad . disminuirEnergia 10 $ personaje

espacio :: Planeta -> Gema
espacio planeta personaje = cambiarPlaneta planeta . disminuirEnergia 20 $ personaje

poder :: Gema
poder personaje = sacarHabilidadSiEsNecesario . disminuirEnergia (energia personaje) $ personaje

tiempo :: Gema
tiempo personaje = reducirEdad . disminuirEnergia 50 $ personaje

gemaLoca :: Gema -> Gema
gemaLoca gema personaje = gema . gema $ personaje

------------------------------------
--------------- AUX --------------
reducirEdad :: Personaje -> Personaje
reducirEdad personaje
  | edad personaje > 36 = personaje {edad = edad personaje `div` 2}
  | otherwise = personaje {edad = 18}

sacarHabilidadSiEsNecesario :: Personaje -> Personaje
sacarHabilidadSiEsNecesario personaje
  | (< 3) (length . habilidades $ personaje) = personaje {habilidades = []}
  | otherwise = personaje

cambiarPlaneta :: Planeta -> Personaje -> Personaje
cambiarPlaneta planetaNuevo personaje =
  personaje
    { planeta = planetaNuevo
    }

disminuirEnergia :: Int -> Personaje -> Personaje
disminuirEnergia puntos personaje =
  personaje
    { energia = energia personaje - puntos
    }

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad habilidadAEliminar personaje =
  personaje
    { habilidades = filter (/= habilidadAEliminar) (habilidades personaje)
    }

---------------------------
-------- Punto 3 ----------
gemasDelInfinito :: Gema -> [Gema]
gemasDelInfinito = repeat

---------------------------
-------- Punto 4 ----------
ejemplo :: Guantelete
ejemplo =
  UnGuantelete
    { material = Goma,
      gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]
    }

---------------------------
-------- Punto 5 ----------
utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas personaje = foldr (\gema p -> gema p) personaje listaGemas

---------------------------
-------- Punto 6 ----------

-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima.

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = buscarGema (gemas guantelete) personaje

buscarGema :: [Gema] -> Personaje -> Gema
buscarGema [n] personaje = n
buscarGema (n : m : ns) personaje
  | (energia . n $ personaje) > (energia . m $ personaje) = buscarGema (m : ns) personaje
  | otherwise = buscarGema (m : ns) personaje

---------------------------
-------- Punto 7 ----------
-- Dada la función generadora de gemas y un guantelete de locos:
-- infinitasGemas :: Gema -> [Gema]
-- infinitasGemas gema = gema:(infinitasGemas gema)

-- guanteleteDeLocos :: Guantelete
-- guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

-- Y la función
-- usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
-- usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher

-- usoLasTresPrimerasGemas si va a poder ejecutar, esto se da gracias a la Lazy Aplicacion que tiene el paradigma. No va a analizar valores hasta sea indispensable para su funcionamiento, por lo que va a usar las tres primeras gemas y nunca va a ver el resto de las gemas.
-- En cambio creo que gemaMasPoderosa no va a correr, va a quedar trabado esto se da porque si necesita analizar todos los valores de la lista para ejecutar.