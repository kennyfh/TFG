
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module KmeansHist where

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
-- import Data.Array.Accelerate.Interpreter as I -- TODO: modificar esto por el interprete de CUDA 
-- import Data.Array.Accelerate.LLVM.PTX


-- Tipos de Datos

-- Punto
type Point a = Vector a

-- Los clusters consisten en la localización del centroide, así como su identificador
type Id = Word32 
type Cluster a = (Id, Vector a)

-- Id de nuestro cluster
idOfCluster :: (Elt a, Elt (Array DIM1 a)) => Exp (Cluster a) -> Exp Id
idOfCluster = A.fst

-- Centroide del cluster
centroidOfCluster :: (Elt a, Elt (Array DIM1 a)) => Exp (Cluster a) -> Exp (Point a)
centroidOfCluster = A.snd

-- Estructura intermedia  que contiene el número de puntos como la suma
-- de cada array con sus respectivos canales.

type PointSum a = (Word32,Vector a)

-- TODO: si da tiempo a implementar esto, podemos realizar otras distancias
-- fold haciendo euclidea, manhatan o cualquiera de esos
distance :: A.Num a => Exp (Point a) -> Exp (Point a) -> Exp a
distance u v =
  let vec1 = unlift u
      vec2 = unlift v
  in A.the $ A.foldAll (+) 0 $ A.map (P.^ 2) $ A.zipWith (-) (use vec1) (use vec2)

findClosestCluster
    :: forall a. (A.RealFloat a, P.RealFloat a)
    => Acc (Vector (Cluster a))
    -> Acc (Vector (Point a))
    -> Acc (Vector Id)
findClosestCluster clusters points =
  A.map (\p -> A.fst $ A.sfoldl (nearest p) z (constant Z) clusters) points
  where
    z = constant (-1, inf)

    nearest :: Exp (Point a) -> Exp (Id, a) -> Exp (Cluster a) -> Exp (Id, a)
    nearest p st c =
      let d  = A.snd st
          d' = distance p (centroidOfCluster c)
      in
      d' A.< d ? ( lift (idOfCluster c, d') , st )


makeNewClusters
    :: forall a. (A.RealFloat a, P.RealFloat a, A.FromIntegral Word32 a)
    => Acc (Vector (Point a))
    -> Acc (Vector (Cluster a))
    -> Acc (Vector (Cluster a))
makeNewClusters points clusters
  = pointSumToCluster
  . makePointSum
  . findClosestCluster clusters 
  $ points

  where
    npts        = size points
    nclusters   = size clusters

    -- Turn the PointSum intermediate structure into the clusters, by averaging
    -- the cumulative (x,y) positions.
    --
    pointSumToCluster :: Acc (Vector (PointSum a)) -> Acc (Vector (Cluster a))
    pointSumToCluster ps =
      A.generate (A.shape ps)
                 (\ix -> lift (A.fromIntegral (unindex1 ix), average (ps ! ix)))

    average :: Exp (PointSum a) -> Exp (Point a)
    average ps =
      let (n, xy) = unlift ps   :: (Exp Word32, Exp (Point a))
          vec = unlift xy
      in
      lift (run $ A.map (/A.fromIntegral n) vec) -- TLM: what if there are no points in the cluster??

    -- Reduce along the rows of 'pointSum' to get the cumulative (x,y) position
    -- and number of points assigned to each centroid.
    --
    makePointSum :: Acc (Vector Id) -> Acc (Vector (PointSum a))
    makePointSum nearest = A.permute addPointSum zeros near input
      where zeros = undefined
            near ix = index1 (A.fromIntegral (nearest ! ix)) -- function index permutation
            input = A.zip (A.fill (A.shape points) (constant 1)) points -- [(1,Vector a), (1,Vector a)....]

    -- 
    addPointSum :: Exp (PointSum a) -> Exp (PointSum a) -> Exp (PointSum a)
    addPointSum x y =
      let (c1, u) = unlift x    :: (Exp Word32, Exp (Point a))
          (c2, v) = unlift y    :: (Exp Word32, Exp (Point a))
          vec1 = unlift u    :: Vector a
          vec2 = unlift v    :: Vector a
      in
        lift (c1+c2, lift (run $ A.zipWith (+) (use vec1) (use vec2)) :: Exp (Point a))


-- To complete the k-means algorithm, we loop repeatedly generating new clusters
-- positions, until the positions converge (or some maximum iteration limit is
-- reached?)
--
kmeans :: forall a. (A.RealFloat a, P.RealFloat a, A.FromIntegral Word32 a)
       => Acc (Vector (Point a))        -- the points to cluster
       -> Acc (Vector (Cluster a))      -- initial cluster positions (guess)
       -> Acc (Vector (Cluster a))
kmeans points clusters
  = A.asnd
  $ A.awhile (A.uncurry keepGoing)
             (\cs -> let (_, old) = unlift cs   :: (Acc (Vector (Cluster a)), Acc (Vector (Cluster a)))
                         new      = makeNewClusters points old
                     in
                     lift (old,new))
             (lift (clusters, makeNewClusters points clusters))
  where
    keepGoing :: Acc (Vector (Cluster a)) -> Acc (Vector (Cluster a)) -> Acc (Scalar Bool)
    keepGoing xs ys
      = A.or
      $ A.zipWith (\c1 c2 -> let vec1 = unlift (centroidOfCluster c1) :: (Point a)
                                 vec2 = unlift (centroidOfCluster c2) :: (Point a)
                             in 
                               A.the A.or $ A.zipWith (\p1 p2 -> let z1 = unlift p1
                                                                     z2 = unlift p2
                                                                 in abs (z1-x2) A.> 0.01) vec1 vec2) xs ys


-- EL número real no infinito más largo 
inf :: forall a. P.RealFloat a => a
inf = P.encodeFloat m n
  where
    a           = undefined :: a
    b           = P.floatRadix a
    e           = P.floatDigits a
    (_, e')     = P.floatRange a
    m           = b P.^ e - 1
    n           = e' - e


test :: IO()
test = do
    -- -- type Point a = (Vector a, Vector a, Vector a)
    let vec1 = fromList (Z:.3) [0,1,2] :: Vector Float
    let vec2 = fromList (Z:.3) [3,4,5] :: Vector Float
    -- let v = run $ fill (constant (Z:.3)) 0 :: Vector Float
    -- let xs = run $ fill (constant (Z:.10)) (constant v)
    let zeros = run $ fill (constant (Z:.10)) (constant vec1) :: Vector (Vector Float)
    print zeros
    putStrLn "Fin de la prueba"
