-- |
-- Module:      : Repa
-- Copyright    : [2022] Kenny Jesús Flores Huamán
-- License      : BSD3
--
-- Maintainer   : Kenny Jesús Flores Huamán <kennyjesus@pm.me>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards,
             MagicHash, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-incomplete-patterns -fno-warn-orphans #-}

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
import qualified Data.Vector.Unboxed            as VU
import Data.Array.Repa.Repr.Unboxed as U
import JuicyRepa
import Juicy
import Control.Parallel
import Data.Array.Repa.Specialised.Dim2
import Data.Array.Repa.Repr.Cursored (makeCursored)
import qualified Data.Vector.Unboxed.Mutable as VM
-- import System.Random
import Control.DeepSeq

-- Instancias para poder realizar Benchmarks
instance Unbox e => NFData (Channel e) where
  rnf = (`deepSeqArray` ())
instance Unbox e => NFData (Array U DIM1 e) where
  rnf = (`deepSeqArray` ())

-- función producto escalar en Repa

-- Versión Unboxed 
-- scalarpRepa :: (Monad m) => Array U DIM1 Float -> Array U DIM1 Float -> m Float
-- scalarpRepa xs ys = sumAllP (R.zipWith (*) xs ys)

testScalarRepa :: IO ()
testScalarRepa = do
    let arr = fromListUnboxed (Z :. 10000000) [1..10000000] :: Array U DIM1 Float
    -- let arr = R.fromFunction (Z :.20) (\( Z :. i) -> i +1) :: Array D DIM1 Float
    -- print arr 
    z <- scalarpRepa arr arr
    print z 

-- Versión genérica
scalarpRepa :: (Shape sh, Unbox a, Num a, Monad m, Source r1 a, Source r2 a) => Array r1 sh a -> Array r2 sh a -> m a
scalarpRepa xs ys = sumAllP (R.zipWith (*) xs ys)

-- Funciones para cambiar tipos de datos en Repa
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


{--
  _   _   _         _                                              
 | | | | (_)  ___  | |_    ___     __ _   _ __    __ _   _ __ ___  
 | |_| | | | / __| | __|  / _ \   / _` | | '__|  / _` | | '_ ` _ \ 
 |  _  | | | \__ \ | |_  | (_) | | (_| | | |    | (_| | | | | | | |
 |_| |_| |_| |___/  \__|  \___/   \__, | |_|     \__,_| |_| |_| |_|
                                  |___/                            
--}

-- Versión Alfa (NO FUNCIONA DEBIDO A QUE ES LENTO)
-- type RGB = (Pixel8,Pixel8,Pixel8)
-- type Img a = Array U DIM2 a
-- type Histogram = Array D DIM1 Int
-- type Histograms = (Histogram,Histogram,Histogram)

-- fst' :: (a,b,c) -> a
-- fst' (a,_,_)=a

-- snd' :: (a,b,c) -> b
-- snd' (_,b,_)=b

-- third' :: (a,b,c) -> c
-- third' (_,_,z) = z

-- computeHistograms :: Histograms -> (Array U DIM1 Int,Array U DIM1 Int,Array U DIM1 Int)
-- computeHistograms (a,b,c)= (R.computeS a,R.computeS b,R.computeS c)

-- doHistogramRGB :: Img RGB -> Histograms
-- doHistogramRGB img =
--     let (Z :. nrows :. ncolums) = R.extent img
--         zero = R.fromFunction (Z :. 256) (\_ -> 0::Int)
--         incElem idx x = RU.unsafeTraverse x id (\l i -> l i + if i==(Z:.fromIntegral idx) then 1 else 0)
--     in Prelude.foldl (\(hstR,hstG,hstB) (rows,colums) ->
--         let val = unsafeIndex img (Z :. rows :. colums)
--             r = fst' val
--             g = snd' val
--             b = third' val
--         in (incElem r hstR, incElem g hstG,incElem b hstB))
--         (zero,zero,zero)
--         [(rows,colums) | rows <- [0..nrows], colums <- [0..ncolums-1]]




{-Tipos de Datos-}
type Histogram = Seq Int
type Histograms = [Histogram]

-- 1º Versión (Cálculo de Histogramas secuencialmente)
generateHistogramsV1 :: ImgRGB Int-> Histograms
generateHistogramsV1 = Prelude.map generateHistogram

-- 2º version (Cálculo de Histogramas de manera paralela)
-- Nivel grano grueso: haz el cálculo del histograma de cada canal en paralelo. 
-- Esto solo llevaría a hacerlo tan solo 3 veces más rápido.

generateHistogramsV2 :: ImgRGB Int-> Histograms
generateHistogramsV2 xs =
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

generateHistogramsV3 :: ImgRGB Int-> [HVector]
generateHistogramsV3 xs =
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
generateRow band i =
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
--   ____     ___    __        __
--  | __ )   ( _ )   \ \      / /
--  |  _ \   / _ \/\  \ \ /\ / / 
--  | |_) | | (_>  <   \ V  V /  
--  |____/   \___/\/    \_/\_/              
 
FORMULA PARA SACAR LA LUMINOSIDAD
    Y' = 0.2989 R + 0.5870 G + 0.1140 B 

De:  http://poynton.ca/notes/colour_and_gamma/ColorFAQ.html#RTFToC11

-}

-- 1º Version usando zipwith
toGrayScaleV1
    :: ImgRGB Pixel8 -- ^ Dada una imagen RGB
    -> IO (Channel Float) -- ^ Devolvemos una imagen 
toGrayScaleV1 img = R.computeP $ R.zipWith (+) b (R.zipWith (+) r g)
    where r = generateComponentB 0 (L.head img)
          g = generateComponentB 1 (img !! 1)
          b = generateComponentB 2 (L.last img)

generateComponentB :: Int -> Channel Pixel8 -> Array D DIM2 Float
generateComponentB ind = R.map (\v -> (fromIntegral (fromIntegral v) / 255)*val)
    where val = [0.2989,0.5870,0.1140] !! ind

-- 2º Version usando zip3
{-# INLINE luminance #-}
luminance :: (Pixel8, Pixel8, Pixel8) -> Float -- ^ Cada terna RGB lo transforma en su luminosidad
luminance (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r) / 255
        g'      = fromIntegral (fromIntegral g) / 255
        b'      = fromIntegral (fromIntegral b) / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11

-- toGrayScale :: ImgRGB Pixel8 -> IO (Channel Float)
toGrayScaleV2
    :: ImgRGB Pixel8 -- ^ Dada una imagen RGB [Channel Float , Channel Float, Channel Float]
    -> IO (Channel Float) -- ^ Devolvemos la luminosidad de nuestra imagen Channel Float
toGrayScaleV2 [r,g,b] = R.computeP
                        . R.map luminance
                        $ U.zip3 r g b
toGrayScaleV2 _ = error "No hay bandas suficientes"
{-# NOINLINE toGrayScaleV2 #-}


{-- 

   ____                               _                     _       _                
  / ___|   __ _   _   _   ___   ___  (_)   __ _   _ __     | |__   | |  _   _   _ __ 
 | |  _   / _` | | | | | / __| / __| | |  / _` | | '_ \    | '_ \  | | | | | | | '__|
 | |_| | | (_| | | |_| | \__ \ \__ \ | | | (_| | | | | |   | |_) | | | | |_| | | |   
  \____|  \__,_|  \__,_| |___/ |___/ |_|  \__,_| |_| |_|   |_.__/  |_|  \__,_| |_|   
                                                                                     
--}

--- Para poder realizar el blur, lo primero que debemos hacer es como vamos a aplicar
-- el filtro de esto -- sigma=1, kernel_radius = 5
-- https://observablehq.com/@jobleonard/gaussian-kernel-calculater
-- [0.06136,0.24477,0.38774,0.24477,0.06136]
-- Como Stencil2 no acepta decimales, nuestra lista se quedará de la siguiente manera
    --[1/16, 4/16,....]
-- 

blurX :: Channel Float -> IO (Channel Float)
blurX img = R.computeP -- Computamos todo de forma paralela (CPU)
            $ R.smap (/ 16) -- un map más eficiente
            $ forStencil2  BoundClamp img
              [stencil2| 1 4 6 4 1 |]
{-# NOINLINE blurX #-}


blurY :: Channel Float -> IO (Channel Float)
blurY img =  R.computeP -- Computamos todo de forma paralela 
             $ R.smap (/16) -- Dividimos cada pixel de la imagen entre 16
             $ forStencil2 BoundClamp img
               [stencil2|    1
                             4
                             6
                             4
                             1 |] -- Usamos un stencil para hacer las convoluciones
{-# NOINLINE blurY #-}

-- Uso de 2 kernels
blurV1 :: Int -> Channel Float -> IO (Channel Float)
blurV1 = go -- Es equivalente a esto : blur steps imgInit = go steps imgInit
    where go 0 !img = return img -- Si ya no podemos hacer más iteraciones, devolvemos la imagen
          go n !img = -- En caso contrario
              do sepx <- blurX img -- Realizamos el difuminado usando el kernel por el eje x 
                 sepy <- blurY sepx -- Realizamos el difuminado usando como kernel del eje y
                 go (n-1) sepy -- Y repetimos todo el proceso 
{-# NOINLINE blurV1 #-}


-- Uso de 2 kernels (Versión sin iteraciones)
-- blurV1 :: Channel Float -> IO (Channel Float)
-- blurV1 = blurX >=> blurY -- blurV1 img = (blurX >=> blurY) img
-- {-# NOINLINE blurV1 #-}


-- Kernel 5x5
-- https://www.opencv-srf.com/2018/03/gaussian-blur.html
-- https://homepages.inf.ed.ac.uk/rbf/HIPR2/gsmooth.htm
-- SIGMA =1
blurV2 :: Int -> Channel Float -> IO (Channel Float)
blurV2 = go
    where go 0 !img = return img
          go n !img = do
                        blurStep <- R.computeP
                                    $ R.smap (/273)
                                    $ forStencil2 BoundClamp img
                                      [stencil2| 1 4  7  4  1 
                                                 4 16 26 16 4
                                                 7 26 41 26 7
                                                 4 16 26 16 4
                                                 1 4  7  4  1 |]

                        go (n-1) blurStep
{-# NOINLINE blurV2 #-}


-- blurV2 :: Channel Float -> IO (Channel Float)
-- blurV2 img = R.computeP
--              $ R.smap (/273)
--              $ forStencil2 BoundClamp img
--                [stencil2| 1 4  7  4  1 
--                           4 16 26 16 4
--                           7 26 41 26 7
--                           4 16 26 16 4
--                           1 4  7  4  1 |]

-- {-# NOINLINE blurV2 #-}


 {-
   __  __                        
 |  \/  |   ___    __ _   _ __  
 | |\/| |  / _ \  / _` | | '_ \ 
 | |  | | |  __/ | (_| | | | | |
 |_|  |_|  \___|  \__,_| |_| |_|
                                 
 -}

-- https://homepages.inf.ed.ac.uk/rbf/HIPR2/mean.htm

meanF
    ::  Channel Float --  ^ Imagen de Entrada
    -> IO (Channel Float) -- ^ Imagen de Salida
meanF img = R.computeP
        $ R.smap (/9)
        $ forStencil2 BoundClamp img
              [stencil2|  1  1  1
                          1  1  1 
                          1  1  1 |]

-- 


{-
  ____            _              _ 
 / ___|    ___   | |__     ___  | |
 \___ \   / _ \  | '_ \   / _ \ | |
  ___) | | (_) | | |_) | |  __/ | |
 |____/   \___/  |_.__/   \___| |_|
                                   
-}
-- https://es.wikipedia.org/wiki/Operador_Sobel#Formulaci%C3%B3n

-- Cálculo de Gx
gradX :: Channel Float -> IO (Channel Float)
gradX img = R.computeP -- Computamos todo de forma paralela 
            $ forStencil2 BoundClamp img
              [stencil2| -1 0 1
                         -2 0 2 
                         -1 0 1 |] -- Realizamos una convolución donde vamos a usar un kernel 3x3 para cada pixel de la imagen
{-# NOINLINE gradX #-}

-- Calculo de Gy
gradY  :: Channel Float -> IO (Channel Float)
gradY img = R.computeP -- Computamos todo de forma paralela
            $ forStencil2 BoundClamp img
              [stencil2| -1 -2 -1
                          0  0  0 
                          1  2  1 |] -- Realizamos una convolución donde vamos a usar un kernel 3x3 para cada pixel de la imagen
{-# NOINLINE gradY #-}
-- Normalmente, Sobel usa 2 kernels 3x3, aunque según wikipedia,
-- cada kernel podemos descomponerlo en otros 2 filtros.

-- -- Magnitude G = Sqrt (Gx^2 + Gy^2)
-- magnitude :: Float -> Float -> Float
-- {-# INLINE magnitude #-}
-- magnitude x y = sqrt (x*x + y*y)

-- Para usar este algoritmo, en primer lugar debemos sacar la luminosidad
-- de la imagen RGB (pasarlo a blanco y negro)
sobel
    :: Channel Float -- ^ Imagen de entrada
    -> IO (Channel Float) -- ^ Imagen de salida tras haberle aplicado el filtro
sobel img = do
    gx <- gradX img -- Calculamos el gradiente de X de la imagen
    gy <- gradY img -- Calculamos el gradiente de Y de la imagen
    R.computeP $ R.zipWith (\x y -> sqrt (x*x + y*y)) gx gy -- Realizamos G = Sqrt (Gx^2 + Gy^2) a cada
                                           -- pixel de la imagen

{-
  _                       _                       
 | |       __ _   _ __   | |   __ _    ___    ___ 
 | |      / _` | | '_ \  | |  / _` |  / __|  / _ \
 | |___  | (_| | | |_) | | | | (_| | | (__  |  __/
 |_____|  \__,_| | .__/  |_|  \__,_|  \___|  \___|
                 |_|                              

-}

-- https://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm
laplace
    :: Channel Float
    -> IO (Channel Float)
laplace img =
    R.computeP
    $ forStencil2 BoundClamp img
        [stencil2|  0 -1  0 
                   -1  4 -1
                    0 -1  0 |]



{--
  _                       _                  _                              __      ____                               _                 
 | |       __ _   _ __   | |   __ _    ___  (_)   __ _   _ __       ___    / _|    / ___|   __ _   _   _   ___   ___  (_)   __ _   _ __  
 | |      / _` | | '_ \  | |  / _` |  / __| | |  / _` | | '_ \     / _ \  | |_    | |  _   / _` | | | | | / __| / __| | |  / _` | | '_ \ 
 | |___  | (_| | | |_) | | | | (_| | | (__  | | | (_| | | | | |   | (_) | |  _|   | |_| | | (_| | | |_| | \__ \ \__ \ | | | (_| | | | | |
 |_____|  \__,_| | .__/  |_|  \__,_|  \___| |_|  \__,_| |_| |_|    \___/  |_|      \____|  \__,_|  \__,_| |___/ |___/ |_|  \__,_| |_| |_|
                 |_|                                                                                                                     

--}

-- Aproximación discreta de la función LoG con sigma =1.4
-- https://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm


-- ERROR: Los stencils en Repa solamente soportan hasta kernels de 7x7
-- laplacianOfGaussian
--     :: Channel Float
--     -> IO (Channel Float)
-- laplacianOfGaussian img = R.computeP
--                           $ forStencil2 BoundClamp img
--                             [stencil2| 0   1  1  2   2   2  1  1  0
--                                        1   2  4  5   5   5  4  2  1
--                                        1   4  5  3   0   3  5  4  1
--                                        2   5  3 -12 -24 -12 3  5  2 
--                                        2   5  0 -24 -40 -24 0  5  2
--                                        2   5  3 -12 -24 -12 3  5  2
--                                        1   4  5  3   0   3  5  4  1
--                                        1   2  4  5   5   5  4  2  1
--                                        0   1  1  2   2   2  1  1  0|]

