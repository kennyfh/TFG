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
luminance :: (Pixel8, Pixel8, Pixel8) -> Float -- ^ Cada terna RGB lo transforma en su luminosidad
{-# INLINE luminance #-}
luminance (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r) / 255
        g'      = fromIntegral (fromIntegral g) / 255
        b'      = fromIntegral (fromIntegral b) / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11

-- toGrayScale :: ImgRGB Pixel8 -> IO (Channel Float)
toGrayScaleV2
    :: ImgRGB Pixel8 -- ^ Dada una imagen RGB 
    -> IO (Channel Float) -- ^ Devolvemos la luminosidad de nuestra imagen
toGrayScaleV2 [r,g,b] = R.computeP . R.map luminance  $ U.zip3 r g b -- A
toGrayScaleV2 _ = error "No se puede hacer debido a que no hay los canales suficientes para realizar el blanco y negro"
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

-- Kernel 5x5
-- https://www.opencv-srf.com/2018/03/gaussian-blur.html
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

-- Magnitude G = Sqrt (Gx^2 + Gy^2)
magnitude :: Float -> Float -> Float
magnitude x y = sqrt (x*x + y*y)

-- Para usar este algoritmo, en primer lugar debemos sacar la luminosidad
-- de la imagen RGB (pasarlo a blanco y negro)
sobel
    :: Channel Float -- ^ Imagen de entrada
    -> IO (Channel Float) -- ^ Imagen de salida tras haberle aplicado el filtro
sobel img = do
    gx <- gradX img -- Calculamos el gradiente de X de la imagen
    gy <- gradY img -- Calculamos el gradiente de Y de la imagen
    R.computeP $ R.zipWith magnitude gx gy -- Realizamos G = Sqrt (Gx^2 + Gy^2) a cada
                                           -- pixel de la imagen

{-
  _                       _                       
 | |       __ _   _ __   | |   __ _    ___    ___ 
 | |      / _` | | '_ \  | |  / _` |  / __|  / _ \
 | |___  | (_| | | |_) | | | | (_| | | (__  |  __/
 |_____|  \__,_| | .__/  |_|  \__,_|  \___|  \___|
                 |_|                              

-}

-- https://iq.opengenus.org/laplacian-filter/
laplace
    :: Monad m
    => Channel Float
    -> m (Channel Float)
laplace img = 
    R.computeP 
    $ forStencil2 BoundClamp img
        [stencil2|  0 -1  0 
                   -1  4 -1
                    0 -1  0 |]

{-
   ____                                   
  / ___|   __ _   _ __    _ __    _   _   
 | |      / _` | | '_ \  | '_ \  | | | |  
 | |___  | (_| | | | | | | | | | | |_| |  
  \____|  \__,_| |_| |_| |_| |_|  \__, |  
                                  |___/  
-}

-- Para la realización de este algoritmo, se ha adaptado gracias a los
-- ejemplos que tiene repa en su repositorio repa-examples
-- https://hackage.haskell.org/package/repa-examples

-- Pasos para realizar el algoritmo Canny:
-- Suavizar la imagen utilizando el filtro gaussiano
-- Conseguir la magnitud y orientación del gradiente
-- Aplicar supresión no máxima (Non-max supression)
-- Aplicar umbral de histéresis (hysteresis threshold)

-- Classification of the output pixel

data Orient = Undef | PosDiag | Vert | NegDiag | Horiz

orient :: Orient -> Float
orient Undef    = 0
orient PosDiag     = 64
orient Vert     = 128
orient NegDiag     = 192
orient Horiz    = 255



data Edge       = None | Weak | Strong

edge :: Edge -> Float
edge None       = 0     :: Float
edge Weak       = 128   :: Float
edge Strong     = 255   :: Float


canny
    :: Int -- ^ Número de pasos a dar
    -> Float -- ^ threshLow
    -> Float -- ^ threshHigh
    -> ImgRGB Pixel8 -- ^ Imagen que le queremos pasar
    -> IO (Channel Pixel8)
canny steps threshLow threshHigh imgInit =
    do imgGrey <- toGrayScaleV2 imgInit -- Pasamos la imagen a blanco y negro
       savePngImage "paso1canny.png" (ImageYF $ exportBW imgGrey) -- Paso CORRECTO
       blurImg <- blurV1 steps imgGrey -- Suavizamos la imagen usando filtro gaussiano
       savePngImage "paso2canny.png" (ImageYF $ exportBW blurImg) -- Paso CORRECTO
       imgGx <- gradX blurImg -- Gradiente X 
       imgGy <- gradY blurImg -- Gradiente Y 
       arrMagDir <- magDir threshLow imgGx imgGy
       let (mag,dir) = U.unzip arrMagDir
       savePngImage "paso3cannymag.png" (ImageYF $ exportBW mag) -- Paso

       print dir
       arrSup <- nonMaximumSuppression threshLow threshHigh arrMagDir
       vecStrong <- selectBestEdges arrSup
       wildfire arrSup vecStrong

-- Función que nos permite obtener la magnitud y la orientación del
-- gradiente 
magDir
    :: Float -- ^ threshLow
    -> Channel Float -- ^ Imagen gradiente X
    -> Channel Float -- ^ Imagen gradiente Y
    -> IO (Channel (Float,Int)) -- ^ Salida con la magnitud y dirección de la imagen
magDir !threshLow gx gy = R.computeP $ R.zipWith magDirAux gx gy
    where magDirAux :: Float -> Float -> (Float,Int)
          magDirAux !x !y = (magnitude x y, orientation x y)
          {-# INLINE magDirAux #-}

          orientation :: Float -> Float -> Int
          orientation !x !y
            -- En caso no llegemos al valor mínimo del threshold
            | x >= negate threshLow, x < threshLow
            , y >= negate threshLow, y < threshLow = round $ orient Undef -- Lo ponemos negro
            | otherwise =
                let theta = atan2 x y
                    alpha  = (theta - (pi/8)) * (4/pi)
                    -- Normalizamos el ángulo entre [0..8)
                    norm = if alpha < 0 then alpha + 8 else alpha
                    -- Según repa-examples, esto debe ir más lento que sacar el valor normal
                    -- usando un test explicito
                    or  =  (64 * (1 + floor norm `mod` 4)) `min` 255
                in or
          {-# INLINE orientation #-}


-- Esta función realiza uno de los pasos más importantes del algoritmo de
-- Detección de bordes Canny, nuestro objetivo es remover los bordes tanto reduntantes
-- como duplicados identificados por el algoritmo de detección Sobel. Por lo que al solo 
-- querer una línea que muestre el borde en vez de tener varias líneas para el mismo borde
-- y esto se consigue mediante el algoritmo de supresión No-max realizado a continuación.
nonMaximumSuppression
    :: Float -- ^ ThreshLow
    -> Float -- ^ ThreshHigh
    -> Channel (Float,Int) -- ^ Array donde cada pixel 
                           -- ^ es la dirección y la orientación
    -> IO (Channel Float) -- Imagen con los bordes limpios
nonMaximumSuppression low high mgdirArr =
    R.computeP
    $ makeBordered2 (R.extent mgdirArr) -- Dimensión del array
                    1
                    (makeCursored (extent mgdirArr) id addDim compPts)
                    (R.fromFunction (extent mgdirArr) (const 0))

    where {-# INLINE compPts #-}
          compPts idx@(sh :. i :. j)
            -- Si no sabemos la orientación, devolvemos 0
            | o == orient Undef = edge None
            -- Si la orientación es horizontal, devolvemos el máximo entre los vecinos horizontales
            | o == orient Horiz = retMax (getMagnitude (sh :. i :. j-1)) (getMagnitude (sh :. i :. j+1))
            -- Si la orientación es Vertical, devolvemos el máximo entre los vecinos verticales
            | o == orient Vert = retMax (getMagnitude (sh :. i-1 :. j)) (getMagnitude (sh :. i+1 :. j))
            -- Si la orientación es la diagonal negativa, entonces devolvemos el máximo de sus vecinos
            | o == orient NegDiag = retMax (getMagnitude (sh :. i-1 :.j-1 )) (getMagnitude (sh :. i+1 :.j+1 ))
            -- Si la orientación es la diagonal positiva, entonces devolvemos el máximo de sus vecinos
            | o == orient PosDiag = retMax (getMagnitude (sh :. i-1 :.j+1 )) (getMagnitude (sh :. i+1 :.j-1 ))
            | otherwise = edge None

            where
                -- orientación del gradiente
                !o = getOrient idx
                -- Magnitud del gradiente
                !m = getMagnitude (Z :. i :. j)

                getOrient = fst . R.unsafeIndex mgdirArr
                getMagnitude  = snd . R.unsafeIndex mgdirArr

                {-# INLINE retMax #-}
                retMax !intensity1 !intensity2
                    | m < round low     = edge None
                    | m < intensity1    = edge None
                    | m < intensity2    = edge None
                    | m < round high    = edge Weak
                    | otherwise         = edge Strong
{-# NOINLINE nonMaximumSuppression #-}


-- Selecciona los índices de los bordes que queremos quedarnos
-- Esto estaría mucho mejor si se pudiera hacer junto al paso anterior,
-- pero como Repa no permite una primitiva mapFilter fusionada,
-- hay que realizar otra función.
selectBestEdges
    ::  Channel Float -- ^ Nuestra imagen después de haberle hecho nonMaxSuppresssion
    -> IO (Array U DIM1 Int) -- ^ Vector que contiene los índices donde se encuentran los bordes
selectBestEdges img =
     let vec = U.toUnboxed img -- Convertimos nuestro array en un vector  O(1)

         match ix = vec `VU.unsafeIndex` ix == edge Strong -- Función booleana que nos dice si
         {-# INLINE match #-}                                                  -- el pixel tiene el valor 255 o no

         process' ix = ix -- Devolvemos el índice donde se encuentra el pixel Strong (valor pix=255)
         {-# INLINE process' #-}

     in selectP match process' (size $ extent img) -- Esta función produce un vector a todos los elementos
                                                   -- de nuestra imagen. Si el pixel cumple el predicado,
                                                   -- vamos a guardar el índice donde se encuentre su posición.
{-# NOINLINE selectBestEdges #-}


wildfire
    :: Channel Float -- ^ Imagen después de haber aplicado Non-max suppressión
    -> Array U DIM1 Int -- ^ Vector que contiene los índices donde se encuentran los bordes fuertes
    -> IO (Channel Pixel8) -- ^ Imagen habiendole aplicado todo el algoritmo Canny
wildfire img arrStrong
 = do   (sh, vec)       <- wildfireIO
        return  $ sh `seq` vec `seq` R.fromUnboxed sh vec

 where  lenImg          = R.size $ R.extent img
        lenStrong       = R.size $ R.extent arrStrong
        shImg           = R.extent img

        wildfireIO
         = do   -- Stack of image indices we still need to consider.
                vStrong  <- R.toUnboxed <$> R.computeUnboxedP (R.delay arrStrong)
                vStrong' <- VU.thaw vStrong
                vStack   <- VM.grow vStrong' (lenImg - lenStrong)

                -- Burn in new edges.
                vImg    <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg vStack lenStrong
                vImg'   <- VU.unsafeFreeze vImg
                return  (R.extent img, vImg')


        burn :: VM.IOVector Pixel8 -> VM.IOVector Int -> Int -> IO ()
        burn !vImg !vStack !top
         | top == 0
         = return ()

         | otherwise
         = do   let !top'               =  top - 1
                n                       <- VM.unsafeRead vStack top'
                let (R.Z R.:. y R.:. x) = R.fromIndex (R.extent img) n

                let {-# INLINE push #-}
                    push ix t =
                      if R.inShape shImg ix
                         then pushWeak vImg vStack ix t
                         else return t

                VM.write vImg n 255
                 >>  push (R.Z R.:. y - 1 R.:. x - 1) top'
                 >>= push (R.Z R.:. y - 1 R.:. x    )
                 >>= push (R.Z R.:. y - 1 R.:. x + 1)

                 >>= push (R.Z R.:. y     R.:. x - 1)
                 >>= push (R.Z R.:. y     R.:. x + 1)

                 >>= push (R.Z R.:. y + 1 R.:. x - 1)
                 >>= push (R.Z R.:. y + 1 R.:. x    )
                 >>= push (R.Z R.:. y + 1 R.:. x + 1)

                 >>= burn vImg vStack

        -- If this ix is weak in the source then set it to strong in the
        -- result and push the ix onto the stack.
        {-# INLINE pushWeak #-}
        pushWeak vImg vStack ix top
         = do   let n           = R.toIndex (R.extent img) ix
                xDst            <- VM.unsafeRead vImg n
                let xSrc        = img `R.unsafeIndex` ix

                if   xDst == 0 && xSrc == edge Weak
                 then do
                        VM.unsafeWrite vStack top (R.toIndex (R.extent img) ix)
                        return (top + 1)

                 else   return top
{-# NOINLINE wildfire #-}


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
    -- img <- readImageIntoRepa "data/images/saitama.png"
    img <- readImageIntoRepa "data/images/lena_color.png"
    {-FIltro Gaussiano-}
    -- blurImg <- mapM (promote >=> blur 4 >=> demote) img
    -- savePngImage "blursaitama.png" (ImageRGB8 $ repaToJuicy blurImg)

    {-Black and White-}
    imgGrey <- toGrayScaleV2 img
    -- savePngImage "blackandwhite.png" (ImageYF $ exportBW blackAndWhite)

    {-Filtro Sobel-}
    output <- sobel imgGrey
    savePngImage "sobelrepa.png" (ImageYF $ exportBW output)

    {-Filtro Laplace-} -- TODO: Revisar porque no funciona
    -- let [r,g,b] = img -- Auxiliar
    -- mask <- R.computeP $ R.map arrBoundMaskFunc (U.zip3 r g b) :: IO (Channel Float)
    -- maskValue <- R.computeP $ R.map arrBoundValueFunc (U.zip3 r g b) :: IO (Channel Float)
    -- lple <- laplace 2 mask maskValue maskValue
    -- savePngImage "laplace.png" (ImageYF $ exportBW lple)

    {--CANNY --}
    -- cannyImg <- canny 1 50 170 img
    -- savePngImage "canny.png"
    {-Histograma (Sequence)-}
    -- hstSequence <- generateHistograms <$> mapM promoteInt img
    -- print hstSequence

    {-Histograma (Vector)-}
    -- hstVector <- generateHists <$> mapM promoteInt img
    -- print hstVector





    putStrLn "El test ha ido correctamente"