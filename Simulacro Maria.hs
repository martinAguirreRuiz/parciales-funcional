-- Las carreras de autos pueden ser muy divertidas, pero tienen consecuencias. En esta edición de parcial vamos a analizar y producir los efectos que sufren los autos al correr una carrera. Los autos se componen de marca, modelo, desgaste (ruedas y chasis, son dos números), velocidad máxima (m/s), y el tiempo de carrera, que lo vamos a considerar inicialmente 0 y tendremos en cuenta luego el uso.

-- Una pista está compuesta de distintas partes (curvas, rectas, boxes), donde cada tramo termina realizando una transformación sobre el auto que la atraviesa.

-- Nota: Maximizar el uso de aplicación parcial, composición y orden superior. No usar recursividad a menos que se indique que está permitido.

-- 1 . Modelar el auto, teniendo en cuenta la información necesaria que lo representa. Y luego representar:
-- a . Auto Ferrari, modelo F50, sin desgaste en su chasis y ruedas, con una velocidad máxima de 65 m/s.
-- b . Auto Lamborghini, modelo Diablo, con desgaste 7 de chasis y 4 de ruedas, con una velocidad máxima de 73 m/s.
-- c . Auto Fiat, modelo 600, con desgaste 33 de chasis y 27 de ruedas, con una velocidad máxima de 44 m/s.

data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgasteChasis :: Float,
    desgasteRuedas :: Float,
    velocidadMaxima :: Float,
    tiempoDeCarrera :: Float
} deriving (Show)

ferrari :: Auto
ferrari = Auto "Ferrari" "F50" 0 0 65 0

lamborghini :: Auto
lamborghini = Auto "Lamborghini" "Diablo" 7 4 73 0

fiat :: Auto
fiat = Auto "Fiat" "600" 33 27 44 0

-- 2. Estado de salud del auto:
-- a .Saber si un auto está en buen estado, esto se da cuando el desgaste del chasis es menor a 40 y el de las ruedas es menor 60.
-- b. Saber si un auto no da más, esto ocurre si alguno de los valores de desgaste es mayor a 80.

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado unAuto = desgasteChasis unAuto < 40 && desgasteRuedas unAuto < 60

noDaMas :: Auto -> Bool
noDaMas unAuto = desgasteChasis unAuto > 80 || desgasteRuedas unAuto > 80

-- 3.  Reparar un Auto: la reparación de un auto baja en un 85% el desgaste del chasis (es decir que si está en 50, lo baja a 7.5) y deja en 0 el desgaste de las ruedas.

repararAuto :: Auto -> Auto
repararAuto unAuto = unAuto {desgasteChasis = desgasteChasis unAuto * 0.15, desgasteRuedas = 0}

-- 4 . Modelar las funciones para representar las distintas partes de una pista, teniendo en cuenta:
-- a .La curva tiene dos datos relevantes: el ángulo y la longitud. Al atravesar una curva, el auto sufre un desgaste en sus ruedas que responde a la siguiente cuenta: 
-- 3 * longitud / ángulo.
-- Ademas suma un tiempo de longitud / ( velocidad máxima / 2 )
-- -----Modelar curvaPeligrosa, que es una curva de ángulo 60 y longitud de 300m
-- -----Modelar curvaTranca, que es una curva de ángulo 110 y longitud de 550m

type Tramo = Auto -> Auto

type Longitud = Float

type Angulo = Float

curva :: Angulo -> Longitud -> Tramo
curva unAngulo unaLongitud unAuto = sumarADesgasteRuedas (desgasteRuedasCurva unAngulo unaLongitud unAuto) . sumarATiempoDeCarrera (tiempoCarreraCurva unaLongitud unAuto) $ unAuto

tiempoCarreraCurva :: Longitud -> Auto-> Float
tiempoCarreraCurva unaLongitud unAuto = unaLongitud / (velocidadMaxima unAuto / 2)

desgasteRuedasCurva :: Angulo -> Longitud -> Auto -> Float
desgasteRuedasCurva unAngulo unaLongitud unAuto = desgasteRuedas unAuto + 3 * unaLongitud / unAngulo

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

-- b . El tramo recto, debido a la alta velocidad se afecta el chasis del auto en una centésima parte de la longitud del tramo.
-- ----Suma un tiempo de longitud / velocidad máxima
-- ----Modelar tramoRectoClassic de 750m
-- ----Modelar tramito de 280m

tramoRecto :: Longitud -> Tramo
tramoRecto unaLongitud unAuto = sumarADesgasteChasis (unaLongitud / 100) . sumarATiempoDeCarrera (tiempoCarreraRecto unaLongitud unAuto) $ unAuto

tiempoCarreraRecto :: Longitud -> Auto-> Float
tiempoCarreraRecto unaLongitud unAuto = unaLongitud / velocidadMaxima unAuto


tramoRectoClassic :: Tramo
tramoRectoClassic = tramoRecto 750

tramito :: Tramo
tramito = tramoRecto 280

sumarATiempoDeCarrera :: Float -> Auto -> Auto
sumarATiempoDeCarrera incremento unAuto = unAuto {tiempoDeCarrera = tiempoDeCarrera unAuto + incremento}

sumarADesgasteRuedas :: Float -> Auto -> Auto
sumarADesgasteRuedas incremento unAuto = unAuto {desgasteRuedas = desgasteRuedas unAuto + incremento}

sumarADesgasteChasis :: Float -> Auto -> Auto
sumarADesgasteChasis incremento unAuto = unAuto {desgasteChasis = desgasteChasis unAuto + incremento}

-- c. Cuando pasa por un tramo Boxes, si está en buen estado, solo pasa por el tramo que compone Boxes, en el caso contrario se repara y suma 10 segundos de penalización al tiempo del tramo.

boxes :: Tramo -> Tramo
boxes unTramo unAuto
    | estaEnBuenEstado unAuto = unTramo unAuto
    | otherwise = sumarATiempoDeCarrera 10 . repararAuto $ unAuto


-- d . Ya sea por limpieza o lluvia a veces hay una parte de la pista (cualquier parte) que está mojada. Suma la mitad de tiempo agregado por el tramo.


mojado :: Tramo -> Tramo
mojado unTramo unAuto = sumarATiempoDeCarrera (cantidadDeIncremento / 2 ) . unTramo $ unAuto
    where cantidadDeIncremento = tiempoDeCarrera unAutoYaPasadoPorTramo - tiempoDeCarrera unAuto
          unAutoYaPasadoPorTramo = unTramo unAuto

-- e. Algunos tramos tienen ripio (sí... está un poco descuidada la pista) y produce el doble de efecto de un tramo normal equivalente, y se tarda el doble en atravesarlo también. Nos aclaran que, por suerte, nunca hay boxes con ripio.

ripio :: Tramo -> Tramo
ripio unTramo = unTramo . unTramo


-- f . Los tramos que tienen alguna obstrucción, además, producen un desgaste en las ruedas de acuerdo a la porción de pista ocupada, siendo 2 puntos de desgaste por cada metro de pista que esté ocupada, producto de la maniobra que se debe realizar al esquivar dicha obstrucción.

obstruccion :: Float -> Tramo -> Tramo
obstruccion metrosObstruccion unTramo  = unTramo . sumarADesgasteRuedas (2 * metrosObstruccion)

-- 5 . Realizar la función pasarPorTramo/2 que, dado un tramo y un auto, hace que el auto atraviese el tramo, siempre y cuando no pase que no da más.

--Le puse de nombre V2 porq no me dejaba de la otra forma 

pasarPorTramoV2 :: Tramo -> Auto -> Auto
pasarPorTramoV2 unTramo unAuto
    | noDaMas unAuto = unAuto
    | otherwise = unTramo unAuto

-- Teniendo en cuenta que una pista es una sucesión de tramos: 
-- Crear la superPista con los siguientes tramos:
--          tramoRectoClassic
--          curvaTranca
--          2 tramitos consecutivos, pero el primero está mojado
--          Curva con ángulo de 80º, longitud 400m; con obstrucción de 2m
--          Curva con ángulo de 115º, longitud 650m
--          Tramo recto de 970m
--          curvaPeligrosa
--          tramito con ripio
--          Boxes con un Tramo Recto de 800m
-- Hacer la función peganLaVuelta/2 que dada una pista y una lista de autos, hace que todos los autos den la vuelta (es decir, que avancen por todos los tramos), teniendo en cuenta que un auto que no da más “deja de avanzar”.

type Pista = [Tramo]

superPista :: Pista
superPista = [tramoRectoClassic, curvaTranca, mojado tramito, tramito , obstruccion 2 (curva 80 400), curva 115 650, tramoRecto 970, curvaPeligrosa, ripio tramito, boxes (tramoRecto 800)]

peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista unosAutos = foldl (\autosDeLaPista unTramo -> map (pasarPorTramoV2 unTramo) autosDeLaPista) unosAutos unaPista

-- 7 . ¡¡Y llegaron las carreras!!
-- a. Modelar una carrera que está dada por una pista y un número de vueltas.
-- b. Representar el tourBuenosAires, una carrera que se realiza en la superPista y tiene 20 vueltas.
-- c. Hacer que una lista de autos juegue una carrera, teniendo los resultados parciales de cada vuelta, y la eliminación de los autos que no dan más en cada vuelta.

data Carrera = Carrera {
    pista :: Pista,
    vueltas :: Int
}

tourBuenosAires :: Carrera
tourBuenosAires = Carrera superPista 20



jugarCarrera :: Carrera -> [Auto] -> [[Auto]]
jugarCarrera unaCarrera unosAutos = take (vueltas unaCarrera) . iterate (filter (not.noDaMas) . peganLaVuelta (pista unaCarrera)) $ unosAutos

--Podria borrar el unosAutos pero lo dejo para mas expresividad

-- Creado por Maria Musante :)


