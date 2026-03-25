{--
Algoritmo de hormigas para el TSP (problema del viajante)

Hecho por: Alberto Peña, Fabio Torres
--}
{--
DATOS DE ENTRADA:
 1º Matriz de adyacencia con los pesos(distancias) (Fichero a parte del que tomamos su informacion haciendo entrada/salida)
 2º Matriz con feromonas (se realiza en una funcion), constanteQ = 10.0 , rho = 0.1 prefijadas.
--}
import System.Random (newStdGen,randomRs) -- Importamos de la libreria System.Random estas funciones para generar numeros aleatorios

-- Previamente tenemos que programar un procesa (IO) para que nos coja los valores de pesos (distancias)
-- TIPOS DE DATOS  (ponemos apodos a los tipos ya predefinidos para no liarnos luego con Int/Float y verlo mas claro).
type Ciutat = Int   
type Distancia = Float
type Feromona = Float
type Probabilidad = Float
data Camino = C [Ciutat]
    deriving(Show,Read)
data Hormiga = H (Camino,Float) 
    deriving(Show,Read)

--Creamos la matriz de feromonas
feromonas :: Int -> [[Feromona]]
feromonas n = replicate n (replicate n 1.0)

procesa :: IO()
procesa = do putStr("Introduce el nombre del archivo: ")
             nombreIn <- getLine
             putStr("Introduce el numero de iteraciones:")
             iteracionesStr <- getLine
             let iteraciones = read iteracionesStr ::Int
             contenido <- readFile nombreIn
             semilla <- newStdGen
             let listaA = randomRs (0,100) semilla --lista infinita de enteros aleatorios entre 0 y 100
             let resultado = ejecutar iteraciones listaA contenido
             putStrLn resultado

ejecutar :: Int-> [Int] -> String -> String 
ejecutar n lAl contenido =  mostrar solFinal
    where lineas = lines contenido
          palabras = map words lineas 
          dists = map (map read) palabras ::[[Distancia]]
          numCiudades = length dists
          ciudades = [1 .. numCiudades] --Las ciudades van a ser 1,2,...,n
          solInicial = H ( C [],10000000000000000.0) --nos ponemos como solucion inicial una muy mala
          solFinal = aco lAl n ciudades dists (feromonas numCiudades) solInicial 
                    
mostrar :: Hormiga -> String
mostrar (H (C sol_camino,sol_coste)) = "El mejor camino encontrado es: " ++ show sol_camino ++
    "\n" ++ "Con un coste total de: "++ show sol_coste

--Paso1 : Construir la matriz de probabilidades
--Dada una ciudad, calculamos la probabilidad de ir al resto de ciudades.
calcProb :: Ciutat -> [Ciutat] -> [[Distancia]] -> [[Feromona]] -> [(Ciutat,Probabilidad)]  --Usamos tupla para que, a la hora de elegir ciudad, saber cuál es.
calcProb actual novisitadas dists feros = (zip novisitadas probas) --Devolvemos lista de tuplas con la ciudad de destino y su probabilidad de llegar.
    where pesos = map (\cand -> calcPeso actual cand dists feros) novisitadas
          pesoTotal = sum pesos --Calculamos el denominador de la formula de probabilidad.
          probas = map (\w -> (w/pesoTotal)*100) pesos -- Finalmente calculamos una lista con las probabilidades de ir a cada ciudad ( en porcentaje)
          
--Calculamos el numerador de la fórmula de probabilidad de ir de la ciudad i a la ciudad j
calcPeso ::  Ciutat -> Ciutat -> [[Distancia]] ->[[Feromona]] -> Float 
calcPeso i j dists feros
    |i == j  = 0.0  
    |otherwise = tau * (1.0/dist)
        where (tau,dist) = ((feros!!(i-1))!!(j-1) ,(dists!!(i-1))!!(j-1))

-- "Acumulamos" las probabilidades para que, a la hora de elegir un número aleatorio, podamos decidir bien por qué camino ir
acumuladorProb :: [(Ciutat,Probabilidad)] -> [(Ciutat,Probabilidad)]
acumuladorProb xs = zip ciudades acumulador
    where probs = map snd xs
          ciudades  = map fst xs
          acumulador = scanl1 (+) probs
          
--Paso 2: Construir el camino que hace la hormiga

--A esta funcion le pasamos una lista de tuplas con las probabilidades "acumuladas" para poder decidir a qué ciudad ir 
eleccionCiudad :: [(Ciutat,Probabilidad)] -> Int -> Ciutat
eleccionCiudad [x] _ = fst x
eleccionCiudad ((ciudad, prob):xs) random
    | (fromIntegral random) <= prob = ciudad    -- Si el numero cae en mi rango, me eligen a mí
    | otherwise  = eleccionCiudad xs random

--Dada una ciudad de partida, construimos un camino que pase por todas las ciudades y vuelva a la ciudad original
caminoHormigas :: [Int] -> Ciutat -> [Ciutat] -> [[Distancia]] -> [[Feromona]] -> Camino
caminoHormigas (l:lAl) actual novisitadas dists feros 
    |novisitadas == [] = C []   -- si la lista de ciudades es vacia devolvemos camino vacio
    |otherwise = C (sigCiudad: restoCamino)
    where  mapaprobas = acumuladorProb $ calcProb actual novisitadas dists feros
           sigCiudad = eleccionCiudad mapaprobas l -- elegimos la siguiente ciudad
           restantes = filter (/= sigCiudad) novisitadas --la quitamos de las ciudades restantes
           (C restoCamino )= caminoHormigas lAl sigCiudad restantes dists feros -- llamamos recursivamente a la función para hacer el resto del camino.

--Paso 3 (ACTUALIZACION DE FEROMONAS) Una vez que las hormigas hayan hecho sus caminos, hay que actualizar las feromonas 
--SUPONEMOS QUE EL GRAFO NO ES DIRIGIDO
--No es el código más eficiente pues hacemos el mismo cálculo en distintas funciones.

longitudCamino :: Camino -> [[Distancia]] -> Float 
longitudCamino (C []) _ = 0.0
longitudCamino (C camino) dists = ( sum distPasos ) + distFinal
    where pasos = zip camino (tail camino) -- hacemos una lista de tuplas que son los vertices que componen las aristas por las que la hormiga se mueve
          distPasos = map (\(x,y) ->(dists !! (x-1)) !! (y-1) ) pasos --buscamos las distancias
          distFinal = (dists !! ((last camino)-1))!! ((head camino)-1) -- Tengo que volver, luego calculo distancia final

-- Calculamos la feromona que añade una hormiga en las aristas por las que pasa
adicionFero :: Ciutat -> Ciutat -> Camino -> [[Distancia]] -> Float
adicionFero i j  camino dists
    |perteneceAlCamino i j camino = 10.0/ lk -- formula de wikipedia (constanteQ)
    |otherwise = 0.0
    where lk = longitudCamino camino dists

--MATANDO MOSCAS A CAÑONAZOS
perteneceAlCamino :: Ciutat -> Ciutat -> Camino -> Bool
perteneceAlCamino i j (C camino) = filtrado1 || filtrado2
        where aristas1  = zip camino (tail camino ++ [head camino]) --aristas del camino en un sentido
              aristas2  = zip (tail camino ++ [head camino]) camino --aristas del camino en otro sentido
              filtrado1 = (filter ( ==(i,j)) aristas1) /= [] --si alguna de estas dos listas no es vacia devuelve True sino False 
              filtrado2 = (filter ( ==(i,j)) aristas2) /= []
 
sumadicionFero :: Ciutat -> Ciutat -> [Camino] -> [[Distancia]] -> Float
sumadicionFero i j caminos dists  = sum add_hormiga
    where add_hormiga = map (\camino -> adicionFero i j camino dists) caminos

--Actualizamos las feromonas de todas las aristas
actualizarFeromonas :: [[Feromona]] -> [Camino] -> [[Distancia]] -> [[Feromona]]
actualizarFeromonas feromonas_antiguas caminos dists = [[ nueva_fero feromonas_antiguas dists caminos i j | j <- indices] | i <- indices]  --lista intensional para generar nueva matriz de feromonas 
    where indices :: [Int]
          indices = [1 .. (length feromonas_antiguas)] --los indices que en la siguiente funcion son las ciudades
          
--Calcula la nueva feromona de una arista concreta.  
nueva_fero :: [[Feromona]] ->[[Distancia]]->[Camino] -> Ciutat -> Ciutat -> Feromona
nueva_fero feromonas_antiguas dists caminos i j 
    | i == j = 0.0
    |otherwise =parte1 + sumarFerosAntiguas --formula de wikipedia
    where tau_original = (feromonas_antiguas !!(i-1))!!(j-1)
          parte1  = (1- 0.1)* tau_original --rho = 0.1
          sumarFerosAntiguas = sumadicionFero i j caminos dists --sumatorio de las feromonas antiguas

--Paso 4 : Realizamos el bucle que utilice todas las funciones programadas anteriormente.
--Queremos hacer una función que se llame hormigasEnCadaCiudad donde ponemos una hormiga en cada ciudad y cada una hace su solucion. Esta funcion devuelve una lista de caminos

--Dados todos los caminos realizados por las hormigas en una iteración, nos quedamos con el que mejor coste tenga
mejorCamino :: [Camino] -> [[Distancia]] -> Hormiga
mejorCamino caminos dists = H (head camino_min) --si encontrasemos varios caminos con mismo coste nos quedamos con el primero.
    where soluciones = map (\x -> (x, longitudCamino x dists)) caminos 
          costes = map snd soluciones
          camino_min = filter (\(x,y) -> (y ==minimum costes )) soluciones
          
-- En cada iteracion de nuestro bucle, deberiamos comprobar si la nueva solucion que obtenemos es mejor/peor que la anterior.
compararSol :: Hormiga -> Hormiga -> Hormiga
compararSol (H (x,y)) (H (z,w))
    | y <= w = H (x,y)
    |otherwise = H (z,w)
    
pasos :: Camino -> [(Ciutat,Ciutat)]
pasos (C xs) = zip xs (tail xs ++ [head xs]) 

--Definimos un nuevo operador que nos permita concatenar caminos 
infixr 6 +++
(+++) :: Camino -> Camino -> Camino
(C xs) +++ (C ys ) = C( xs++ys)

--Creamos una funcion que haga un camino desde cada ciudad y se quede con el mejor camino.
hormigasEnCadaCiudad :: [Int] -> [Ciutat] -> [Ciutat] -> [[Distancia]] -> [[Feromona]]-> [Camino] 
hormigasEnCadaCiudad _ [] _ _ _ = []
hormigasEnCadaCiudad lAl(ciudad:resto) todasciudades dists feros = (uncamino : restoCaminos)
    where no_visitadas = filter (/= ciudad) todasciudades
          uncaminoSinFinal = caminoHormigas lAl ciudad no_visitadas dists feros
          uncamino = (C [ciudad]) +++ uncaminoSinFinal
          restoCaminos = hormigasEnCadaCiudad (drop (length dists) lAl)  resto todasciudades dists feros

--Funcion principal. Llama a todas las funciones anteriores y hace el bucle de ejecucion mediante recursion.      
aco :: [Int] -> Int -> [Ciutat] -> [[Distancia]] -> [[Feromona]] -> Hormiga -> Hormiga 
aco lAl 0 _ _ _ mejorSol = mejorSol
aco lAl n ciudades dists feros mejorSol= aco (drop ((length dists)*(length dists)*2) lAl)  (n-1) ciudades dists nuevasFeros nuevamejorSol --nos aseguramos de quitar todos los aleatorios ya usados quitando una cota superior.
    where caminosGenerados = hormigasEnCadaCiudad lAl ciudades ciudades dists feros
          mejordelaIteracion = mejorCamino caminosGenerados dists --Aqui nace la hormiga buena
          nuevamejorSol = compararSol mejorSol mejordelaIteracion
          nuevasFeros = actualizarFeromonas feros caminosGenerados dists
