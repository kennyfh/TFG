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
import Data.Vector as V hiding (mapM)
import Data.Array.Repa.Repr.Unboxed as U
import JuicyRepa
import Juicy
import Control.Parallel
import System.Random
{--
  _____                _   
 |_   _|   ___   ___  | |_ 
   | |    / _ \ / __| | __|
   | |   |  __/ \__ \ | |_ 
   |_|    \___| |___/  \__|
                           
--}

test :: IO ()
test = do
    putStrLn "Iniciando Test Fichero Repa.hs"
    img <- readImageIntoRepa "saitama.png"
    {-FIltro Gaussiano-}
    -- asd <- mapM (promote >=> passes 2 gaussFilter >=> demote) img
    -- savePngImage "axxa.png" (ImageRGB8 $ repaToJuicy asd)

    {-Histograma (Sequence)-}
    -- hstSequence <- generateHistograms <$> mapM promoteInt img
    -- print hstSequence

    {-Histograma (Vector)-}
    -- hstVector <- generateHists <$> mapM promoteInt img
    -- print hstVector

    {-Black and White-}
    blackAndWhite <- toGrayScale img
    savePngImage "blackandwhite.png" (ImageYF $ exportBW blackAndWhite)

    img2 <- readImageIntoRepa "NZ6DR.jpg"
    rojo <- promote $  L.head img2
    aa <- passes 4 edgeFilter rojo
    aa2 <- demote aa
    savePngImage "example.png" (ImageY8 $ exportBand aa2)


    putStrLn "El test ha ido correctamente"
    

promote :: Monad m => Channel Pixel8 -> m (Channel Float)
promote  = computeP . R.map func
    where {-# INLINE func #-}
          func :: Pixel8 -> Float
          func x = fromIntegral x
{-# NOINLINE promote #-}


demote :: Monad m => Channel Float -> m (Channel Pixel8)
demote =  computeP . R.map func
    where {-# INLINE func #-}
          func :: Float -> Pixel8
          func x = fromIntegral (truncate x :: Int)
{-# NOINLINE demote #-}

promoteInt :: Channel Pixel8 -> IO (Channel Int)
promoteInt = computeP . R.map func
    where  {-# INLINE func #-}
           func :: Pixel8 -> Int 
           func x  = fromIntegral x
{-# NOINLINE promoteInt #-}

-- promotesToInt :: ImgRGB Pixel8 -> IO (ImgRGB Int)
-- promotesToInt = mapM promoteInt
--     where promoteInt :: Channel Pixel8 -> IO (Channel Int)
--           promoteInt = computeP . R.map func
--             where  func :: Pixel8 -> Int 
--                    func x  = fromIntegral x

-- demotesInt :: ImgRGB Int -> IO (ImgRGB Pixel8)
-- demotesInt = mapM demoteInt
--     where demoteInt :: Channel Int -> IO (Channel Pixel8)
--           demoteInt = computeP . R.map func
--             where func :: Int -> Pixel8
--                   func x = fromIntegral x

{--
  _   _   _         _                                              
 | | | | (_)  ___  | |_    ___     __ _   _ __    __ _   _ __ ___  
 | |_| | | | / __| | __|  / _ \   / _` | | '__|  / _` | | '_ ` _ \ 
 |  _  | | | \__ \ | |_  | (_) | | (_| | | |    | (_| | | | | | | |
 |_| |_| |_| |___/  \__|  \___/   \__, | |_|     \__,_| |_| |_| |_|
                                  |___/                            
--}

{-Tipos de Datos-}
type Histogram = Seq Int
type Histograms = [Histogram]

-- 1º Versión (Cálculo de Histogramas secuencialmente)
-- generateHistograms :: ImgRGB -> Histograms
-- generateHistograms = Prelude.map generateHistogram

-- 2º version (Cálculo de Histogramas de manera paralela)
-- Nivel grano grueso: haz el cálculo del histograma de cada canal en paralelo. 
-- Esto solo llevaría a hacerlo tan solo 3 veces más rápido.

generateHistograms :: ImgRGB Int-> Histograms
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

generateHists :: ImgRGB Int-> [HVector]
generateHists xs =
    let bs = Prelude.map generateHist xs
        cs = bs `using` parList rdeepseq
        in cs

generateHist :: Channel Int -> HVector
generateHist band = pfold (V.zipWith (+)) (generateRows band)

generateRows :: Channel Int -> [HVector]
generateRows band =
    let (Z :. w :. h) = R.extent band
        bs = Prelude.map (generateRow band) [0..w-1]
        cs = bs `using` parList rdeepseq
        in cs


generateRow :: Channel Int -> Int -> Vector Int
generateRow band i=
    let zero = V.replicate 256 0 -- O(n)
        fila =  R.toList $ R.slice band  (Any :. (i::Int) :. All) -- Seleccionamos la Fila [12,3,4,4,5]
        l = L.zip fila [1,1..]
    in V.accum (+) zero l -- Crearemos el histograma con los valores correspondientes


-- Función que nos hace un fold de manera paralela
pfold :: (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend xs  = (ys `par` zs) `pseq` (ys `mappend` zs) where
  len = L.length xs
  (ys', zs') = L.splitAt (len `div` 2) xs
  ys = pfold mappend ys'
  zs = pfold mappend zs'

{- 
  ____     ___    __        __
 | __ )   ( _ )   \ \      / /
 |  _ \   / _ \/\  \ \ /\ / / 
 | |_) | | (_>  <   \ V  V /  
 |____/   \___/\/    \_/\_/   
                              
FORMULA PARA SACAR LA LUMINOSIDAD
    Y' = 0.2989 R + 0.5870 G + 0.1140 B 
    (https://en.wikipedia.org/wiki/Grayscale)
-}

-- 1º Version usando zipwith
-- toGrayScale :: ImgRGB Int -> IO (Channel Float)
-- toGrayScale img = R.computeP $ R.zipWith (+) b (R.zipWith (+) r g)
--     where r = generateComponentB 0 (L.head img)
--           g = generateComponentB 1 (img !! 1)
--           b = generateComponentB 2 (L.last img)

-- generateComponentB :: Int -> Channel Int -> Array D DIM2 Float
-- generateComponentB ind = R.map (\v -> (fromIntegral (fromIntegral v ::Int) / 255)*val)
--     where val = [0.2989,0.5870,0.1140] !! ind

-- 2º Version usando zip3

-- floatLuminanceOfRGB8 :: (Float, Float, Float) -> Float
-- {-# INLINE floatLuminanceOfRGB8 #-}
-- floatLuminanceOfRGB8 (r, g, b) = (r / 255) * 0.3 + (g / 255) * 0.59 + (b / 255) * 0.11

-- https://hackage.haskell.org/package/repa-algorithms-3.4.0.2/docs/src/Data-Array-Repa-Algorithms-Pixel.html
luminance :: (Pixel8, Pixel8, Pixel8) -> Float
{-# INLINE luminance #-}
luminance (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r) / 255
        g'      = fromIntegral (fromIntegral g) / 255
        b'      = fromIntegral (fromIntegral b) / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11

toGrayScale :: ImgRGB Pixel8 -> IO (Channel Float)
toGrayScale [r,g,b] = R.computeP . R.map luminance  $ U.zip3 r g b
toGrayScale _ = error "No se puede hacer debido a que no hay los canales suficientes para realizar el blanco y negro"
{-# NOINLINE toGrayScale #-}


{-- 

   ____                               _                     _       _                
  / ___|   __ _   _   _   ___   ___  (_)   __ _   _ __     | |__   | |  _   _   _ __ 
 | |  _   / _` | | | | | / __| / __| | |  / _` | | '_ \    | '_ \  | | | | | | | '__|
 | |_| | | (_| | | |_| | \__ \ \__ \ | | | (_| | | | | |   | |_) | | | | |_| | | |   
  \____|  \__,_|  \__,_| |___/ |___/ |_|  \__,_| |_| |_|   |_.__/  |_|  \__,_| |_|   
                                                                                     
--}
type Filter = Channel Float -> IO (Channel Float)

applyStencil :: Stencil DIM2 Float -> Filter
applyStencil st = computeP . mapStencil2 (BoundConst 0) st
{-# NOINLINE applyStencil #-}


passes :: Int -> Filter -> Filter
passes 1 filter = filter
passes n filter = filter >=> passes (n-1) filter


normalize :: Float -> Filter
normalize n = computeP . R.map (/ n)


gaussFilter :: Filter
gaussFilter = applyStencil gaussKernel >=> normalize 159


edgeFilter :: Filter
edgeFilter =  applyStencil edgeKernel

{-Kernels -}
edgeKernel :: Stencil DIM2 Float
edgeKernel =
        [stencil2| 0 1 0
                   1 -4 1
                   0 1 0 |]


gaussKernel :: Stencil DIM2 Float
gaussKernel =
    [stencil2| 2 4 5 4 2
               4 9 12 9 4
               5 12 15 12 5
               4 9 12 9 4
               2 4 5 4 2 |]

--Filtro Media
meanKernel :: Stencil DIM2 Float
meanKernel =
    [stencil2|  1 1 1
                1 1 1 
                1 1 1 |]




-- --Generate a infinite random coordinates
-- randCoords :: StdGen -> Int -> Int -> [(Int,Int)]
-- randCoords a w h = (rnd1,rnd2) : randCoords g2 w h
--     where (rnd1, g1) = randomR (0, w) a
--           (rnd2, g2) = randomR (0, h) g1


