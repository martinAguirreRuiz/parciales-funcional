# Parcial tomado por Scarpa en 2019

La gente del bien se junto para derrotar al mal, y ahora nuestro programa Haskell esta repleto de superheroes. De ellos conocemos su nombre, su vida, su planeta de origen, su artefacto predilecto y su villano enemigo. De este artefacto sabemos el nombre y cuanto daño sufrio.

Los villanos no se querian quedar atras, y de ellos conocemos su nombre, planeta de origen y su arma.

1. Modelar los siguientes personajes:
   - A. Superhoes
    - Iron-Man: Tony Stark, tiene 100 puntos de vida y nacio en la Tieera, Su artefacto es un traje con 12 ptos de dano y su enemigo es thanos
    - Thor: Su nombre completo es Thor Odinson, tiene 300 ptos de vida y viene desde Asgard. Utiliza la Stormbreaker, que no esta danada y su enemigo es Loki
   - B. Villanos 
     - Thanos: Nacio en titan y porta el guantelete del infinito
     - Loki: es menos conocido como Loki Laufeyson, su planeta de origen es Jotunheim y su arma es un cetro con 20% de efectividad
2. Definir las siguientes armas:
   - Guantelete del Infinito: Al ser utilizado contra un superheroe, su vida disminuye un 80%
   - Cetro, dado un porcentaje de efectividad, le disminuye la vida al superheroe en dicho porcentaje, ademas si el superheroe es terricola, se rompe su artefacto. Cuando se rompe, se suman 30 ptos al dano sufrido, y a su nombre se le agrega "machacado" al final
3. Saber si un villano y un superheroe son antagonistas. Esto ocurre cuando el villano es el enemigo del superheroe o cuando ambos son oriundos del mismo planeta 
4. Hacer q un superheroe sea atacado por un grupo de villanos. Se pueden dar los siguientes escenarios:
   1. Si el villano es el enemigo del superheroe, este ultimo sale ileso de la batalla
   2. Contra cualquier otro villano, recibe el dano que provoca su arma
5. Conocer que superheroes sobreviven frente a un villano. Quienes sobreviven son aquellos que, luego de ser atacados por el villano, tienen al menos 50 ptos de vida. Dsp de tal hazaña, al nombre de los sobrevivientes se les agrega el prefijo ¨Super¨
6. Hogar, dulce hogar! Hacer que un conjunto de superheroes vuelva a su casa: aquellos que sibrevivan al ataque de Thanos se van a descansar. Al descansar, su vida aumenta en 30 ptos y su artefacto se arregla por completo. No splo se eliminan sus puntos de dano, si el artefacto estaba machacado se le debe sacar
7. Saber si un villano es debil ante un grupo de superheroes. Esto ocurre cuando es antagonista de todos ellos t ninguno tiene su artefacto machacado
8. Dr Strange es un superheroe q se llama Sthepen Strange, oriundo de la Tierra. Tiene 60 ptos de vida, usa la capa de levitacion (que no esta danada) y su enemigo es Thanos. Obtener una lsita de sus infinitos clones, donde cada clon se llama igual que el pero con su numero de clon como sufijo (stephen strange 1, stephen strange 2...)
9. Responder a las siguientes preguntas justificando adecuandamente
   1.  Dada la lista de infinitos clones de Dr Strange, podemos obtener los nombres de quienes sobrevivieron a Thanos?
   2.  Dada una lista infinita de villanos y otra de infinitos superheroes, podemos saber si hay al menos 4 pares que sean antagonistas?
