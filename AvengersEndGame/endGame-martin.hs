-------------------
----- PUNTO 1 -----
-------------------

data SuperHeroe = UnSuperHeroe{
    nombreH   :: String,
    vida      :: Float,
    planetaH  :: String,
    artefacto :: Artefacto,
    enemigo   :: Villano
}

type Artefacto = (Nombre, Danio)
type Nombre    = String
type Danio     = Float

data Villano = UnVillano{
    nombreV  :: String,
    planetaV :: String,
    arma     :: Arma
}
-- a)

ironMan :: SuperHeroe 
ironMan = UnSuperHeroe{
    nombreH   = "Tony Stark",
    vida      = 100,
    planetaH  = "Tierra",
    artefacto = ("Traje", 12),
    enemigo   = thanos
}

thor :: SuperHeroe 
thor = UnSuperHeroe{
    nombreH   = "Thor Odinson",
    vida      = 300,
    planetaH  = "Asgard",
    artefacto = ("Stormbreaker", 0),
    enemigo   = loki
}

-- b)

thanos :: Villano
thanos = UnVillano{
    nombreV  = "Thanos",
    planetaV = "Titan",
    arma     = guanteleteDelInfinito
}

loki :: Villano
loki = UnVillano{
    nombreV  = "Loki Laufeyson",
    planetaV = "Jotunheim",
    arma     = cetro 0.2
}

-------------------
----- PUNTO 2 -----
-------------------

type Arma = SuperHeroe -> SuperHeroe 

guanteleteDelInfinito :: Arma
guanteleteDelInfinito unSuperHeroe = quitarVida (vida unSuperHeroe * 0.8) unSuperHeroe

quitarVida :: Float -> SuperHeroe -> SuperHeroe
quitarVida valorAQuitar unSuperHeroe = unSuperHeroe {vida = vida unSuperHeroe - valorAQuitar}

cetro :: Float -> Arma
cetro unaEfectividad unSuperHeroe = efectoPorTerricola . quitarVida (vida unSuperHeroe * unaEfectividad) $ unSuperHeroe

efectoPorTerricola :: SuperHeroe -> SuperHeroe
efectoPorTerricola unSuperHeroe 
    | planetaH unSuperHeroe == "Tierra" = romperArtefacto unSuperHeroe
    | otherwise                         = unSuperHeroe

romperArtefacto :: SuperHeroe -> SuperHeroe
romperArtefacto unSuperHeroe = unSuperHeroe {artefacto = (nuevoNombre, nuevoDanio)}
    where
        nuevoNombre = (++ "machacado") . fst . artefacto $ unSuperHeroe
        nuevoDanio  = (+ 30) . snd . artefacto $ unSuperHeroe
    
-------------------
----- PUNTO 3 -----
-------------------

sonAntagonistas :: SuperHeroe -> Villano -> Bool
sonAntagonistas unSuperHeroe unVillano = sonOriundos unVillano unSuperHeroe || esEnemigo unVillano unSuperHeroe

esEnemigo :: Villano -> SuperHeroe -> Bool
esEnemigo unVillano unSuperHeroe = (== nombreV unVillano) . nombreV . enemigo $ unSuperHeroe

sonOriundos :: Villano -> SuperHeroe -> Bool
sonOriundos unVillano unSuperHeroe = (== planetaV unVillano) . planetaH $ unSuperHeroe

-------------------
----- PUNTO 4 -----
-------------------

esAtacado :: SuperHeroe -> [Villano] -> SuperHeroe
esAtacado unSuperHeroe listaVillanos = foldr atacaUno unSuperHeroe listaVillanos

atacaUno ::  Villano -> SuperHeroe -> SuperHeroe
atacaUno unVillano unSuperHeroe
    | esEnemigo unVillano unSuperHeroe = unSuperHeroe
    | otherwise                        = arma unVillano unSuperHeroe

-------------------
----- PUNTO 5 -----
-------------------

sobrevivenFrenteA :: Villano -> [SuperHeroe] -> [SuperHeroe]
sobrevivenFrenteA unVillano listaHeroes = map agregarPrefijo . filter (sobreviveFrenteA unVillano) $ listaHeroes

sobreviveFrenteA :: Villano -> SuperHeroe -> Bool
sobreviveFrenteA unVillano unSuperHeroe = (> 50) . vida . atacaUno unVillano $ unSuperHeroe

agregarPrefijo :: SuperHeroe -> SuperHeroe
agregarPrefijo unSuperHeroe = unSuperHeroe {nombreH = "Super " ++ nombreH unSuperHeroe}

-------------------
----- PUNTO 6 -----
-------------------

vuelvenACasa :: [SuperHeroe] -> [SuperHeroe]
vuelvenACasa listaHeroes = map arreglarArtefacto . map (aumentarVida 30) . sobrevivenFrenteA thanos $ listaHeroes

aumentarVida :: Float -> SuperHeroe -> SuperHeroe
aumentarVida valorASumar unSuperHeroe = unSuperHeroe {vida = vida unSuperHeroe + valorASumar}

arreglarArtefacto :: SuperHeroe -> SuperHeroe
arreglarArtefacto unSuperHeroe = unSuperHeroe {artefacto = (nombreArtefactoNuevo . fst . artefacto $ unSuperHeroe , 0)}


nombreArtefactoNuevo :: Nombre -> Nombre
nombreArtefactoNuevo unNombre
    | estaMachacado unNombre = take (length unNombre - 10) unNombre
    | otherwise     = unNombre

estaMachacado :: Nombre -> Bool
estaMachacado unNombre = (== "machacado") . reverse . take 9 . reverse $ unNombre

-------------------
----- PUNTO 7 -----
-------------------

esDebil :: Villano -> [SuperHeroe] -> Bool
esDebil unVillano listaHeroes = all (`sonAntagonistas` unVillano) listaHeroes && all (estaMachacado . fst . artefacto) listaHeroes

-------------------
----- PUNTO 8 -----
-------------------

-- 8. Dr Strange es un superheroe q se llama Sthepen Strange, oriundo de la Tierra. Tiene 60 ptos de vida, usa la capa de levitacion (que no esta danada) y su enemigo es Thanos. Obtener una lsita de sus infinitos clones, donde cada clon se llama igual que el pero con su numero de clon como sufijo (stephen strange 1, stephen strange 2...)

drStrange :: SuperHeroe 
drStrange = UnSuperHeroe{
    nombreH   = "Stephen Strange",
    vida      = 60,
    planetaH  = "Tierra",
    artefacto = ("Capa de levitación", 0),
    enemigo   = thanos
}

infinitosClones :: [SuperHeroe]
infinitosClones = iterate (aumentarSufijo 0) drStrange

aumentarSufijo :: Int -> SuperHeroe -> SuperHeroe
aumentarSufijo unValor unSuperHeroe = unSuperHeroe {nombreH = nombreH unSuperHeroe ++ show (unValor + 1)}

infinitosMartines = iterate (aumentarSufijo' 1) martin

aumentarSufijo' unValor unString = unString ++ show (unValor + 1)
martin = "Martin"

-------------------
----- PUNTO 9 -----
-------------------

-- 9. Responder a las siguientes preguntas justificando adecuandamente
--    1.  Dada la lista de infinitos clones de Dr Strange, podemos obtener los nombres de quienes sobrevivieron a Thanos?
--    2.  Dada una lista infinita de villanos y otra de infinitos superheroes, podemos saber si hay al menos 4 pares que sean antagonistas?

-- 1. No podremos obtenerla, puesto que todos los drStrange sobreviven, la lista seguirá iterando intentando devolvernos los valores, pero nunca terminará.

-- 2. En este segundo caso sí podríamos porque, por más que las listas sean infinitas, al hacer uso del lazy evaluation, basta con encontrar 4 pares para dejar de utilizarla, y por ende, se terminaría su uso y no se quedaría tildado evaluando infinitamente.

