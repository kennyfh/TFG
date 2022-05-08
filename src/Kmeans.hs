
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Kmeans () where

-- El objetivo de utilizar K-means es el poder agrupar todos los histogramas similares
-- en los k clusters que tenemos, por lo que nuestros centroides serán los fotogramas
-- claves que tendremos. 
-- Este algoritmo ha sido adaptado de los ejemplos de accelerate-examples
-- como también del libro "Parallel and Concurrent Programming in
-- Haskell", (c) Simon Marlow, 2013 para que funcione con histogramas
-- siguiendo el artículo "Video Summarization Using Clustering.  T. Chheng"


-- Load modules
import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I -- TODO: modificar esto por el interprete de CUDA 


-- Tipos de Datos

-- Punto
type Point a = (Vector a, Vector a, Vector a)

-- Los clusters consisten en la localización del centroide, así como su identificador
type Id = Word32 
type Cluster a = (Id, (Vector a, Vector a, Vector a))

-- Id de nuestro cluster
idOfCluster :: (Elt a, Elt (Array DIM1 a)) => Exp (Cluster a) -> Exp Id
idOfCluster = A.fst

-- Centroide del cluster
centroidOfCluster :: (Elt a, Elt (Array DIM1 a)) => Exp (Cluster a) -> Exp (Point a)
centroidOfCluster = A.snd

-- Estructura intermedia  que contiene el número de puntos como la suma
-- de cada array con sus respectivos canales.

type PointSum a = (Word32,(Vector a, Vector a, Vector a))


manhattan :: (A.Num a, Unlift Exp (Acc (Array DIM1 a), Acc (Array DIM1 a), Acc (Array DIM1 a))) =>  Exp (Point a) ->  Exp (Point a) -> Acc (Vector a)
manhattan p1 p2 = 
    let (x1,y1,z1) = unlift p1
        (x2,y2,z2) =  unlift p2
        in A.zipWith3 (\x y z -> x+y+z) (A.map A.abs (A.zipWith (-) x1 x2)) (A.map A.abs (A.zipWith (-) y1 y2)) (A.map A.abs (A.zipWith (-) z1 z2))

findClosestCluster clusters points = undefined 



main :: IO()
main = do
    -- type Point a = (Vector a, Vector a, Vector a)
    let vec1 = fromList (Z:.3) [0,1,2] :: Vector Int
    let vec2 = fromList (Z:.3) [3,4,5] :: Vector Int
    let vec3 = fromList (Z:.3) [6,7,8] :: Vector Int
    let p1 = (vec1,vec2,vec3) :: Point Int
    let p2 = (vec3,vec2,vec1) :: Point Int
    putStrLn "Fin de la prueba"
