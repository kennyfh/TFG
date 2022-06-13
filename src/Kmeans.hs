{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module KmeansSeg (Point, Cluster, Id, kmeans) where
module Kmeans () where

import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX

-- https://media.githubusercontent.com/media/tmcdonell/tmcdonell.github.io/master/papers/TrevorMcDonell_PhD_Thesis.pdf
-- para poder arreglar el pointsum

{-
La implementación la vamos a realizar para puntos de 3 dimensiones

-}

-- -- Tipo de dato punto
-- type Point a = (a,a,a)

-- -- Tipo de datos Clusters : consisten en la ubicación del centroide,
-- -- así como su identificador

-- -- Identificador
-- type Id = Word32
-- -- Clusters
-- type Cluster a = (Id, Point a)

-- -- Métodos para quedarnos con el identificador o con el centroide del cluster

-- idOfCluster :: Elt a => Exp (Cluster a) -> Exp Id
-- idOfCluster = A.fst -- Es equivalente a idOfCluster cluster = A.fst cluster

-- centroidOfCluster :: Elt a => Exp (Cluster a) -> Exp (Point a)
-- centroidOfCluster = A.snd -- Es equivalente a centroidOfCluster cluster = A.snd cluster

-- {-
-- PointSum: va a ser una estructura intermedia que va a contener el número de puntos
-- que tiene el conjunto como la suma de coordenadas "x", "y" y "z".

-- -}

-- type PointSum a = (Word32, (a,a,a))

-- -- Método que nos devuelve la distancia euclídea de 2 puntos
-- -- TODO: PARA MEJORAR ESTO, podemos realizar distintas distancias
-- -- para pasárselas al algoritmo

-- distance :: A.Num a => Exp (Point a) -> Exp (Point a) -> Exp a
-- distance u v = 
--     let (x1,y1,z1) =  unlift u -- desencapsulamos Exp (Point a) para quedarnos con (Point a)
--         (x2,y2,z2) =  unlift v
--     -- Realizamos el cálculo del valor 
--     in (x1-x2) P.^ (2::Int) + (y1-y2) P.^ (2::Int) + (z1-z2) P.^ (2::Int)


-- {-
-- Por cada punto dado, devuelve el cluster id del cual ese punto está
-- más cerca

-- -}

-- findClosestCluster 
--     :: forall a. (A.RealFloat a, P.RealFloat a )
--     => Acc (Vector (Cluster a)) -- ^ Vector con todos los clusters
--     -> Acc (Vector (Point a)) --  ^ Vector con cada punto dado
--     -> Acc (Vector Id) -- ^ Devolvemos el cluster Id MÁS CERCANO a ese punto
-- findClosestCluster clusters points = 
--     A.map (\point -> A.fst $ A.sfoldl (nearest point) z (constant z) clusters) points
--     -- Para cada punto del vector, vamos a ir buscando la identificación del cluster más cercano
--     -- esto es posible gracias a la función nearest
--     where
--         z = constant (-1,inf) -- Creamos una variable auxiliar para ir viendo los estados

--         -- Función auxiliar que nos va a ayudar a encontrar el id del cluster
--         -- más cercano para el punto
--         nearest :: Exp (Point a) -> Exp (Id, a) -> Exp (Cluster a) -> Exp (Id, a)
--         nearest p st c = 
--             let d = A.snd st -- Nos quedamos con la distancia de la que se encontraba el cluster anterior con el punto que estamos buscando su identificador
--                 d' = distance p (centroidOfCluster c) -- Calculamos la distancia de este cluster con el punto al que estamos buscando su cluster
--             -- Si la distancia del cluster actual con respecto al punto es menor al nuevo cluster, devolvemos el ID del nuevo cluster con su distancia a la que se
--             -- encuentra, si no, devolvemos el cluster anterior con su distancia correspondiente
--             in d' A.< d ? ( lift (idOfCluster c, d') , st )

-- -- Nota para entender la función (?):
-- -- https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/src/Data.Array.Accelerate.Prelude.html#%3F%7C
-- -- (?) :: Exp Bool -> (Acc a, Acc a) -> Acc a
-- -- Dado una expresión booleana, si se cumple devolvemos la primera parte de la tupla, si no, devolvemos su segunda componente
-- -- Ejemplo simplificado : 3 < 2 ? ([1,2,3],[4,5,6]) = [4,5,6]


-- {-
-- Dado un vector de puntos y un vector de clusters, primero localizamos el grupo más cercano
-- a cada punto , asignamos ese punto a su grupo más cercano y calculamos el centroide del cluster.
-- Esto produce nuevas localizaciones de los centroides.

-- -}

-- makeNewClusters
--     :: forall a. (A.RealFloat a, P.RealFloat a,  A.FromIntegral Word32 a)
--     => Acc (Vector (Point a)) -- ^ Vector de puntos
--     -> Acc (Vector (Cluster a)) -- ^ Vector de clusters
--     -> Acc (Vector (Cluster a)) -- ^ Devolvemos los clusters actualizados con los nuevos centroides
-- makeNewClusters points clusters
--     = pointSumToCluster
--     . makePointSum
--     . findClosestCluster clusters
--     $ points

--     where 
--         -- Sacamos el tamaño de los vectores que vamos a realizar
--         npts = size points
--         nclusters = size clusters

--         -- Convierta la estructura intermedia PointSum en grupos,
--         -- promediando las posiciones acumulativas (x,y,z).
--         pointSumToCluster :: Acc (Vector (PointSum a)) -> Acc (Vector (Cluster a))
--         pointSumToCluster ps = 
--             A.generate (A.shape ps)
--                        (\ix -> lift (A.fromIntegral (unindex1 ix), average (ps ! ix)))
            
--         -- Media de todos los puntos 
--         average :: Exp (PointSum a) -> Exp (Point a)
--         average ps = 
--             let (n,xy) = unlift ps :: (Exp Word32, Exp (Point a))
--                 (x,y,z) = unlift xy
--             in 
--                 lift (x / A.fromIntegral n, y / A.fromIntegral n, z / A.fromIntegral  n)

--         -- Reducr a lo largo de las filas de PointSum para obtener la
--         -- posición acumulativa (x,y,z)y el número de puntos asignados
--         -- a cada centroide
--         makePointSum :: Acc (Vector (PointSum a))
--         -- makePointSum :: Acc (Vector Id) -> Acc (Vector (PointSum a))
--         makePointSum nearest = A.permute addPointSum zeros near input
--             where
--                     zeros   = A.fill (constant (Z:.nclusters)) (constant (0,(0,0,0)))
--                     input   = A.zip (A.fill (A.shape points) (constant 1)) points
--                     near ix = index1 (A.fromIntegral (nearest ! ix))
        
    

--         addPointSum :: Exp (PointSum a) -> Exp (PointSum a) -> Exp (PointSum a)
--         addPointSum x y = 
--             let (id1,p1) = unlift x :: (Exp Word32, Exp (Point a)) -- Desencapsulamos el primer pointsum
--                 (id2,p2) = unlift y :: (Exp Word32, Exp (Point a)) -- Desencapsulamos el seundo pointsum
--                 (x1,y1,z1) = unlift p1 :: (Exp a, Exp a ,Exp a) -- sacamos sus coordenadas de cada punto
--                 (x2,y2,z2) = unlift p2 :: (Exp a, Exp a ,Exp a)
--             in 
--                 lift (id1+id2,lift (x1+x2,y1+y2,z1+z2) ::Exp (Point a))


-- --- Algoritmo Kmedias

-- kmeans :: forall a. (A.RealFloat a, P.RealFloat a, A.FromIntegral Word32 a)
--        => Acc (Vector (Point a))        -- the points to cluster
--        -> Acc (Vector (Cluster a))      -- initial cluster positions (guess)
--        -> Acc (Vector (Cluster a))
-- kmeans points clusters
--   = A.asnd
--   $ A.awhile (A.uncurry keepGoing)
--              (\cs -> let (_, old) = unlift cs   :: (Acc (Vector (Cluster a)), Acc (Vector (Cluster a)))
--                          new      = makeNewClusters points old
--                      in
--                      lift (old,new))
--              (lift (clusters, makeNewClusters points clusters))
--   where
--     keepGoing :: Acc (Vector (Cluster a)) -> Acc (Vector (Cluster a)) -> Acc (Scalar Bool)
--     keepGoing xs ys
--       = A.or
--       $ A.zipWith (\c1 c2 -> let (x1,y1,z1) = unlift (centroidOfCluster c1)
--                                  (x2,y2,z2) = unlift (centroidOfCluster c2)
--                              in
--                              abs (x1-x2) A.> 0.01 A.|| abs (y1-y2) A.> 0.01 A.|| abs (z1-z2) A.> 0.01) xs ys




-- -- El número real non-infinito más largo
-- inf :: forall a. P.RealFloat a => a
-- inf = P.encodeFloat m n
--   where
--     a           = undefined :: a
--     b           = P.floatRadix a
--     e           = P.floatDigits a
--     (_, e')     = P.floatRange a
--     m           = b P.^ e - 1
--     n           = e' - e
