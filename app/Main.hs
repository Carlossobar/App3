-- mago.hs – Recorrido del mago por el bosque usando funciones puras

module Main where
import Data.List (nub)

-- Tipos personalizados para claridad
type Bosque = [[Int]]
type Pos = (Int, Int)
type Camino = [Pos]

-- Bosque de ejemplo: matriz de 3x3
bosqueEjemplo :: Bosque
bosqueEjemplo = 
  [ [2, -3, 1, 4]
  , [-5, 4, 0, 2]
  , [1, 3, 2, 3]
  , [0, 0, 0, 0]
  ]

-- HITO 2: Obtener el valor de la runa en una celda
valorDeRuna :: Bosque -> Pos -> Int
valorDeRuna bosque (i, j) = (bosque !! i) !! j

-- Obtener dimensiones del bosque
dimensionesBosque :: Bosque -> (Int, Int)
dimensionesBosque bosque = (length bosque, length (head bosque))

-- Verificar si una posición está dentro del bosque
posicionValida :: Bosque -> Pos -> Bool
posicionValida bosque (i, j) =
  let (filas, cols) = dimensionesBosque bosque
  in i >= 0 && i < filas && j >= 0 && j < cols

-- Verificar si un movimiento es válido
movimientoValido :: Pos -> Pos -> Camino -> Bool
movimientoValido (x1, y1) (x2, y2) camino =
  let dx = x2 - x1
      dy = y2 - y1
      movimientoBasico = (dx, dy) `elem` [(0,1), (1,0), (1,1)]  -- derecha, abajo, diagonal
      movimientoExtra = (dx, dy) `elem` [(0,-1), (-1,0)]        -- izquierda, arriba
      noRevisita = not ((x2, y2) `elem` camino)
  in movimientoBasico || (movimientoExtra && noRevisita)

-- Obtener todos los movimientos posibles desde una posición
movimientosPosibles :: Bosque -> Pos -> Camino -> [Pos]
movimientosPosibles bosque pos@(x, y) camino =
  let candidatos = [(x, y+1), (x+1, y), (x+1, y+1), (x, y-1), (x-1, y)]  -- derecha, abajo, diagonal, izquierda, arriba
      validos = filter (posicionValida bosque) candidatos
  in filter (\p -> movimientoValido pos p camino) validos

-- HITO 3: Calcular energía luego de un paso
-- Movimiento horizontal/vertical cuesta 1, diagonal cuesta 2
-- Trampa (runa = 0) resta 3 adicional
energiaPaso :: Int -> Pos -> Pos -> Bosque -> Int
energiaPaso energiaActual (x1, y1) (x2, y2) bosque =
  let runa = valorDeRuna bosque (x2, y2)
      costoMovimiento = if x1 /= x2 && y1 /= y2 then 2 else 1
      penalizacion = if runa == 0 then 3 else 0
  in energiaActual - costoMovimiento + runa - penalizacion


-- HITO 4: Calcular energía total de un camino completo
energiaTotal :: Int -> Camino -> Bosque -> Int
energiaTotal energiaInicial [] _ = energiaInicial
energiaTotal energiaInicial [_] _ = energiaInicial
energiaTotal energiaInicial (a:b:resto) bosque =
  let nuevaEnergia = energiaPaso energiaInicial a b bosque
  in energiaTotal nuevaEnergia (b:resto) bosque

-- HITO 5: Verificar si un camino es válido (termina con energía >= 0)
caminoValido :: Int -> Camino -> Bosque -> Bool
caminoValido energiaInicial camino bosque =
  energiaTotal energiaInicial camino bosque >= 0

-- Encontrar todos los caminos posibles desde una posición hasta el final
encontrarCaminos :: Bosque -> Int -> Pos -> Pos -> Camino -> [Camino]
encontrarCaminos bosque energia inicio fin caminoActual
  | energia < 0 = []
  | inicio == fin = [caminoActual]
  | otherwise =
      let siguientesPosiciones = movimientosPosibles bosque inicio caminoActual
          siguientesCaminos pos =
            let nuevaEnergia = if null caminoActual
                              then energia
                              else energiaPaso energia (head caminoActual) pos bosque
            in encontrarCaminos bosque nuevaEnergia pos fin (pos:caminoActual)
      in concatMap siguientesCaminos siguientesPosiciones

-- Encontrar el mejor camino (el que maximiza la energía final)
encontrarMejorCamino :: Bosque -> Int -> [Camino]
encontrarMejorCamino bosque energiaInicial =
  let (filas, cols) = dimensionesBosque bosque
      inicio = (0, 0)
      fin = (filas - 1, cols - 1)
      todosLosCaminos = nub (encontrarCaminos bosque energiaInicial inicio fin [inicio])
      caminosValidos = filter (\c -> caminoValido energiaInicial c bosque) (map reverse todosLosCaminos)
      energias = map (\c -> energiaTotal energiaInicial c bosque) caminosValidos
      maxEnergia = if null energias then -1 else maximum energias
  in filter (\c -> energiaTotal energiaInicial c bosque == maxEnergia) caminosValidos


-- Función principal con ejemplos
main :: IO ()
main = do
  let energiaInicial = 5
  
  putStrLn "Bosque:"
  print bosqueEjemplo
  
  putStrLn "\nBuscando el mejor camino..."
  let mejoresCaminos = encontrarMejorCamino bosqueEjemplo energiaInicial
  
  if null mejoresCaminos
    then putStrLn "No se encontraron caminos válidos"
    else do
      putStrLn "Mejores caminos encontrados:"
      mapM_ (\camino -> do
        putStrLn $ "\nCamino: " ++ show camino
        putStrLn $ "Energía final: " ++ show (energiaTotal energiaInicial camino bosqueEjemplo)
        ) mejoresCaminos
