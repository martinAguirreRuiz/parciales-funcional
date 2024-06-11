
import Data.List (genericLength, find)
import Text.Show.Functions


-------------------------------------
---------------Punto 1---------------
-------------------------------------

data Superheroe = Superheroe {
    nombre :: String,
    vida :: Float,
    planeta :: String,
    artefacto :: Artefacto,
    villanoEnemigo :: Villano
}  deriving (Show)

data Artefacto = Artefacto{   
    nombreArtefacto :: String,
    danio :: Float
}  deriving (Show)

data Villano = Villano {
    nombreVillano :: String,
    planetaVillano :: String,
    arma :: Arma
} deriving (Show)

type Arma = Superheroe -> Superheroe 

ironman :: Superheroe
ironman = Superheroe {
    nombre = "Tony Stark",
    vida = 100,
    planeta = "Tierra",
    artefacto = Artefacto "Traje" 12,
    villanoEnemigo = thanos
}

thor :: Superheroe
thor = Superheroe {
    nombre = "Thor Odinson",
    vida = 300,
    planeta = "Asgard",
    artefacto = Artefacto "Stormbreaker" 0,
    villanoEnemigo = loki
}

thanos :: Villano
thanos = Villano {
    nombreVillano = "Thanos",
    planetaVillano = "Titan",
    arma = guanteleteDelInfinito
}

loki :: Villano
loki = Villano {
    nombreVillano = "Loki Laufeyson",
    planetaVillano = "Jotunheim",
    arma = cetro 20
}


-------------------------------------
---------------Punto 2---------------
-------------------------------------

guanteleteDelInfinito :: Arma
guanteleteDelInfinito = reducirVidaUnPorcentaje 80

cetro :: Float -> Arma
cetro unPorcentajeDeEfectividad = romperArtefactoSiEsTerricola . reducirVidaUnPorcentaje unPorcentajeDeEfectividad 

romperArtefactoSiEsTerricola :: Superheroe -> Superheroe
romperArtefactoSiEsTerricola unSuperheroe
    | esTerricola unSuperheroe = romperArtefacto unSuperheroe
    | otherwise = unSuperheroe

esTerricola :: Superheroe -> Bool
esTerricola unSuperheroe = planeta unSuperheroe == "Tierra"

romperArtefacto :: Superheroe -> Superheroe
romperArtefacto unSuperheroe = unSuperheroe {artefacto = machacharArtefacto $ artefacto unSuperheroe}

machacharArtefacto :: Artefacto -> Artefacto
machacharArtefacto unArtefacto = unArtefacto {danio = danio unArtefacto + 30, nombreArtefacto = nombreArtefacto unArtefacto ++ " machacado"}

reducirVidaUnPorcentaje :: Float -> Superheroe -> Superheroe
reducirVidaUnPorcentaje unPorcentaje unSuperheroe = unSuperheroe {vida = vida unSuperheroe * (1 - unPorcentaje/100)}


-------------------------------------
---------------Punto 3---------------
-------------------------------------

sonAntagonistas :: Superheroe -> Villano -> Bool
sonAntagonistas unSuperheroe unVillano = esEnemigo unSuperheroe unVillano || sonDelMismoPlaneta unSuperheroe unVillano

esEnemigo :: Superheroe -> Villano -> Bool
esEnemigo unSuperheroe unVillano = (nombreVillano . villanoEnemigo) unSuperheroe == nombreVillano unVillano

sonDelMismoPlaneta :: Superheroe -> Villano -> Bool
sonDelMismoPlaneta unSuperheroe unVillano = planeta unSuperheroe == planetaVillano unVillano

-------------------------------------
---------------Punto 4---------------
-------------------------------------

serAtacadoPorMuchosVillanos :: Superheroe -> [Villano] -> Superheroe
serAtacadoPorMuchosVillanos = foldr serAtacadoPorUnVillano

serAtacadoPorUnVillano :: Villano -> Superheroe -> Superheroe
serAtacadoPorUnVillano unVillano unSuperheroe
    | esEnemigo unSuperheroe unVillano = unSuperheroe
    | otherwise = arma unVillano unSuperheroe

-------------------------------------
---------------Punto 5---------------
-------------------------------------

losQueSobreviven :: [Superheroe] -> Villano -> [Superheroe]
losQueSobreviven unosSuperheroes =  agregarSuperASusNombres . filtrarPorAlMenos50deVida . todosSonAtacadosPorUnVillano unosSuperheroes 

todosSonAtacadosPorUnVillano :: [Superheroe] -> Villano -> [Superheroe]
todosSonAtacadosPorUnVillano unosSuperheroes unVillano = map (serAtacadoPorUnVillano unVillano) unosSuperheroes

filtrarPorAlMenos50deVida :: [Superheroe] -> [Superheroe]
filtrarPorAlMenos50deVida = filter ((>=50) . vida)

agregarSuperASusNombres :: [Superheroe] -> [Superheroe]
agregarSuperASusNombres = map agregarSuperAlNombre

agregarSuperAlNombre :: Superheroe -> Superheroe
agregarSuperAlNombre unSuperheroe = unSuperheroe {nombre = "Super " ++ nombre unSuperheroe}

-------------------------------------
---------------Punto 6---------------
-------------------------------------

volverACasa :: [Superheroe] -> [Superheroe]
volverACasa = queVayanADescansar . flip losQueSobreviven thanos

queVayanADescansar :: [Superheroe] -> [Superheroe]
queVayanADescansar = map irADescansar

irADescansar :: Superheroe -> Superheroe
irADescansar unSuperheroe =  unSuperheroe {vida = vida unSuperheroe + 30, artefacto = repararArtefacto $ artefacto unSuperheroe}

repararArtefacto :: Artefacto -> Artefacto
repararArtefacto unArtefacto = unArtefacto {danio = 0, nombreArtefacto = sacarMachacadoDelNombre (nombreArtefacto unArtefacto)}

sacarMachacadoDelNombre :: String -> String
sacarMachacadoDelNombre unNombre
    | diceMachacadoEnElNombre unNombre = take (length unNombre - 10) unNombre
    | otherwise = unNombre

diceMachacadoEnElNombre :: String -> Bool
diceMachacadoEnElNombre unNombre = drop (length unNombre - 10) unNombre == " machacado"

-------------------------------------
---------------Punto 7---------------
-------------------------------------

esDebilAnteUnosSuperheroes :: Villano -> [Superheroe] -> Bool
esDebilAnteUnosSuperheroes unVillano unosSuperheroes = all (flip sonAntagonistas unVillano) unosSuperheroes && all (not . estaMachacado. artefacto) unosSuperheroes

estaMachacado :: Artefacto -> Bool
estaMachacado unArtefacto = diceMachacadoEnElNombre (nombreArtefacto unArtefacto)

-------------------------------------
---------------Punto 8---------------
-------------------------------------

drStrange :: Superheroe
drStrange = Superheroe {
    nombre = "Stephen Strange 1",
    vida = 60,
    planeta = "Tierra",
    artefacto = Artefacto "Capa de Levitacion" 0,
    villanoEnemigo = thanos
}

listaDeClonesDelDrStrange :: [Superheroe]
listaDeClonesDelDrStrange = iterate (nuevoDrStrange 0) drStrange

nuevoDrStrange :: Int -> Superheroe -> Superheroe
nuevoDrStrange valorSufijo unDrStrange = unDrStrange {nombre = init (nombre unDrStrange) ++ show (1+valorSufijo)}


-------------------------------------
---------------Punto 9---------------
-------------------------------------

-- Responder a las siguientes preguntas justificando adecuandamente
-- Dada la lista de infinitos clones de Dr Strange, podemos obtener los nombres de quienes sobrevivieron a Thanos?

-- No, no podemos obtener los nombres de quienes sobrevivieron a Thanos, ya que la funcion va a intentar hacer el ataque de thanos a toda la lista, pero nunca terminara porque esta es infinita, nunca dando un resultado

-- Dada una lista infinita de villanos y otra de infinitos superheroes, podemos saber si hay al menos 4 pares que sean antagonistas?

-- Si, podemos saber si hay al menos 4 pares que sean antagonistas, debido al Lazy Evaluation, buscara 4 pares que lo cumplan y no seguira buscando mas, por lo que no se quedara en un loop infinito, a menos que nunca encuentre los 4 pares.
