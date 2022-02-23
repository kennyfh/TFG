{-# LANGUAGE QuasiQuotes #-}
module Repa where

import Data.Array.Repa as R
import Data.List as L
import Data.Sequence as S
import Codec.Picture
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Control.Monad
import Control.Parallel.Strategies
import System.TimeIt
import Data.Vector as V
import JuicyRepa
import Juicy
import Control.Parallel
test :: IO ()
test = do
    -- Prueba Histogramas
    img <- readImageIntoRepa "saitama.png"
    print $ generateHists img
    -- let xs = R.computeS $ R.fromFunction (Z:.25) (\(Z:.i) -> i :: Int) ::(Array U DIM1 Int)
    -- let asd = R.extract (Z :. 0) (Z :. 5) xs
    -- let channel  = img !! 1
    -- print $ R.extent channel
    -- let asd = generateHistograms img
    -- let asd = generateHistograms img
    -- timeIt $ print $ generateHistograms img
    -- alpha <- toGrayScale img
    -- -- print alpha
    -- let du = ImageRGB8 $ repaToJuicy img
    -- savePngImage "saitamanew.png" du

    {--Pruebas 1 canal --}
    -- img <- readImageJuicy "saitama.png"
    -- channel <- channeljuicyToRepa $ extractChannel 0 img
    -- let asd = generateH channel
    -- putStrLn "AAAAAAAAAA"
    -- print asd
    -- gaussian <- gGaussianChannel channel
    -- savePngImage "saitamablur.png" (ImageY8 $ channelToJcy gaussian)

    -- savePngImage "saitamagauss.png" (ImageRGB8 $ repaToJuicy xs)

    putStrLn "Fin de test"



{-HISTOGRAM -}

{-Tipos de Datos-}
type Histogram = Seq Int
type Histograms = [Histogram]

-- 1º Versión (Cálculo de Histogramas secuencialmente)
-- generateHistograms :: ImgRGB -> Histograms
-- generateHistograms = Prelude.map generateHistogram

-- 2º version (Cálculo de Histogramas de manera paralela)
-- Nivel grano grueso: haz el cálculo del histograma de cada canal en paralelo. 
-- Esto solo llevaría a hacerlo tan solo 3 veces más rápido.

generateHistograms :: ImgRGB -> Histograms
generateHistograms xs =
    let bs = Prelude.map generateHistogram xs
        cs = bs `using` parList rdeepseq
        in cs

generateHistogram :: Channel Int -> Histogram
generateHistogram c =
    let (Z :. w :. h)  = R.extent c
        zero = S.replicate 256 0 -- O(logn)
    in L.foldl' (\hst (x,y) ->
        let val = R.unsafeIndex c (Z :. x :. y)
            cont = hst `S.index` val
        in S.update val (cont+1) hst)
        zero
        [(x,y) | x<-[0..w-1],y<-[0..h-1]]

-- 3º Version Histograma granuelado fino

{-- Nivel grano fino: haz el cálculo de cada fila de la imagen en paralelo. Además, hazlo también para cada canal en paralelo.
De esta forma, si nos fijamos en un canal, obtendrías tantos histogramas como filas tenga la imagen. Después, queda hacer
una suma de cada entrada de los histogramas para obtener el histograma final. 
La operación de suma también se puede paralelizar ya que es un reduce.
Una solución que generalice lo anterior y que haga un histograma por cada N filas. 
Después en tu experimentos juega con esa N, a ver cuando va más rápido.
--}
-- Para esta versión necesitamos una Estructura distinta de datos
type HVector = Vector Int
type HVectors = [HVector]

generateHists :: ImgRGB -> [HVector]
generateHists xs =
    let bs = Prelude.map generateH xs
        cs = bs `using` parList rdeepseq
        in cs

generateH :: Channel Int -> HVector
generateH band =  L.foldr1 (V.zipWith (+)) (generateR band) -- TODO: Pararelizar esto

generateR :: Channel Int -> [HVector]
generateR band = [generateR2 band i | i<-[0..w-1]]
    where (Z :. w :. h) =  R.extent band
    

generateR2 :: Channel Int -> Int -> Vector Int
generateR2 band i=
    let zero = V.replicate 256 0 -- O(n)
        fila =  R.toList $ R.slice band  (Any :. (i::Int) :. All) -- Seleccionamos la Fila
        l = L.zip fila [1,1..] 
    in V.accum (+) zero l -- Crearemos el histograma con los valores correspondientes


{- Blanco y negro 

FORMULA PARA SACAR LA LUMINOSIDAD
    Y' = 0.2989 R + 0.5870 G + 0.1140 B 
    (https://en.wikipedia.org/wiki/Grayscale)
-}

toGrayScale :: ImgRGB -> IO ImgA
toGrayScale img = R.computeP $ R.zipWith (+) b (R.zipWith (+) r g)
    where r = generateComponentB 0 (L.head img)
          g = generateComponentB 1 (img !! 1)
          b = generateComponentB 2 (L.last img)

generateComponentB :: Int -> Channel Int -> Array D DIM2 Float
generateComponentB ind = R.map (\v -> (fromIntegral (fromIntegral v ::Int) / 255)*val)
    where val = [0.2989,0.5870,0.1140] !! ind


{-- Filtro gaussiano
--}
type Filter a = Channel a -> IO (Channel a)
type Filters a = [Filter a]

gaussStencil :: Stencil DIM2 Int
gaussStencil =
    [stencil2| 2 4 5 4 2
               4 9 12 9 4
               5 12 15 12 5
               4 9 12 9 4
               2 4 5 4 2 |]

applyStencil :: Stencil DIM2 Int -> Filter Int
applyStencil stencil = computeP . mapStencil2 (BoundConst 0) stencil

passes :: Int -> Filter Int -> Filter Int
passes 1 filter = filter
passes n filter = filter >=> passes (n-1) filter