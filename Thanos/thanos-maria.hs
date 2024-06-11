import Data.List (genericLength, find)
import Text.Show.Functions

-- Primera parte
-- Los enanos de Nidavellir nos han pedido modelar los guanteletes que ellos producen en su herrería. Un guantelete está hecho de un material (“hierro”, “uru”, etc.) y sabemos las gemas que posee. También se sabe de los personajes que tienen una edad, una energía, una serie de habilidades (como por ejemplo “usar espada”, “controlar la mente”, etc), su nombre y en qué planeta viven. Los fabricantes determinaron que cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- y su material es “uru”, se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes. Por ejemplo si tenemos un universo en el cual existen ironMan, drStrange, groot y wolverine, solo quedan los dos primeros que son ironMan y drStrange. Si además de los 4 personajes estuviera viudaNegra, quedarían también ironMan y drStrange porque se considera la división entera.

-- Punto 1: (2 puntos) Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

data Personaje = Personaje {
    edad :: Int,
    energia :: Float,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: String
} deriving(Show)

type Habilidad = String

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving Show

type Gema = Personaje -> Personaje

type Universo = [Personaje]

intentaChasquido :: Guantelete -> Universo -> Universo
intentaChasquido guantelete universo
    | material guantelete == "uru" && length (gemas guantelete) == 6 = chasquidoUniverso universo
    | otherwise = universo


chasquidoUniverso :: Universo -> Universo
chasquidoUniverso universo = take (length universo `div` 2) universo

-- Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
-- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any ((< 45) . edad)

energiaTotal :: Universo -> Float
energiaTotal = sum . map energia . filter ((> 1) . length . habilidades)

-- Segunda parte
-- A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, el poseedor puede utilizar el poder del mismo contra un enemigo, es decir que puede aplicar el poder de cada gema sobre el enemigo. Las gemas del infinito fueron originalmente parte de la entidad primordial llamada Némesis, un ser todopoderoso del universo anterior quién prefirió terminar su existencia en lugar de vivir como la única conciencia en el universo. Al morir, dio paso al universo actual, y el núcleo de su ser reencarnó en las seis gemas: 
-- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
-- El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía. 
-- El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.
-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).
-- El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.
-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

-- Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada. 

gemaMente :: Float -> Gema
gemaMente = bajarEnergia

bajarEnergia :: Float -> Personaje -> Personaje
bajarEnergia unValor unPersonaje = unPersonaje {energia = energia unPersonaje - unValor}

gemaAlma :: Habilidad -> Gema
gemaAlma unaHabilidad = bajarEnergia 10 . quitarHabilidad unaHabilidad

quitarHabilidad :: Habilidad -> Personaje -> Personaje
quitarHabilidad unaHabilidad unPersonaje = unPersonaje {habilidades = filter (/= unaHabilidad) (habilidades unPersonaje)}

gemaEspacio :: String -> Gema
gemaEspacio unPlaneta = bajarEnergia 20 . transportarAUnPlaneta unPlaneta

transportarAUnPlaneta :: String -> Personaje -> Personaje
transportarAUnPlaneta unPlaneta unPersonaje = unPersonaje {planeta = unPlaneta}

gemaPoder :: Gema
gemaPoder unPersonaje = bajarEnergia (energia unPersonaje) . quitarHabilidadesSiCorresponde $ unPersonaje

quitarHabilidadesSiCorresponde :: Personaje -> Personaje
quitarHabilidadesSiCorresponde unPersonaje
    | (length . habilidades) unPersonaje <= 2 = unPersonaje {habilidades = []}
    | otherwise = unPersonaje

gemaTiempo :: Gema
gemaTiempo = bajarEnergia 50 . reducirEdad

reducirEdad :: Personaje -> Personaje
reducirEdad unPersonaje = unPersonaje {edad = max 18 (edad unPersonaje `div` 2)}

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema . unaGema


-- Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete "goma" [gemaTiempo, gemaAlma "usar Mjolnir", gemaLoca $ gemaAlma "programación en Haskell"]

-- Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.

type Enemigo = Personaje

utilizar :: [Gema] -> Enemigo -> Enemigo
utilizar unasGemas unEnemigo = foldr usarGema unEnemigo unasGemas

usarGema :: Gema -> Enemigo -> Enemigo
usarGema unaGema unEnemigo = unaGema unEnemigo


-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMasPoderosa :: Enemigo -> Guantelete -> Gema
gemaMasPoderosa _ (Guantelete _ [unaGema]) = unaGema
gemaMasPoderosa unEnemigo (Guantelete _ (unaGema:otraGema:masGemas)) = gemaMasPoderosa unEnemigo (Guantelete "" (esMasPoderosa unEnemigo unaGema otraGema : masGemas))

esMasPoderosa :: Enemigo -> Gema -> Gema -> Gema
esMasPoderosa unEnemigo unaGema otraGema
    | energia (unaGema unEnemigo) < energia (otraGema unEnemigo) = unaGema
    | otherwise = otraGema


-- Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas gemaTiempo)

otroGuantelete :: Guantelete
otroGuantelete = Guantelete "madera" [gemaMente 10, gemaAlma "usar Mjolnir", gemaEspacio "Tierra", gemaPoder, gemaTiempo, gemaLoca $ gemaAlma "programación en Haskell"]

-- Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

punisher :: Enemigo
punisher = Personaje 30 100 ["usar Mjolnir", "programación en Haskell"] "Punisher" "Tierra"

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- gemaMasPoderosa punisher guanteleteDeLocos
-- No se puede ejecutar, ya que la función gemaMasPoderosa espera un Guantelete con al menos dos gemas, y guanteleteDeLocos tiene infinitas gemas. Por lo tanto, la función no terminaría de ejecutarse nunca.
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
-- Se puede ejecutar, ya que la función utilizar toma las primeras 3 gemas del guantelete y las aplica sobre el enemigo. Aunque el guantelete tenga infinitas gemas, la función solo toma las primeras 3, por lo que no hay problema de que el guantelete tenga infinitas gemas.
