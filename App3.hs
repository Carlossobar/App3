module Main where

type Bosque = [[Int]]
type Pos = (Int, Int)
type Camino = [Pos]

bosqueEjemplo :: Bosque
bosqueEjemplo = 
  [ [2, -3, 1]
  , [-5, 4, 0]
  , [1, 3, 2]
  ]

main :: IO ()
main = do
  putStrLn "Bosque mágico:"
  print bosqueEjemplo
