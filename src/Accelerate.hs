{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-orphans #-}
module Accelerate where

import JuicyAccelerate
import Codec.Picture
import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Interpreter as I -- TODO: modificar esto por el interprete de CUDA 
import Data.Array.Accelerate.LLVM.PTX as PTX
import Prelude as P
import Control.Monad ((>=>))

-- Instancias para poder realizar benchmarks



promoteInt :: Acc (Matrix Pixel8) -> Acc (Matrix Int)
promoteInt = A.map A.fromIntegral

demoteInt :: Acc (Matrix Int) -> Acc (Matrix Pixel8)
demoteInt =  A.map A.fromIntegral

promoteFloat :: Acc (Matrix Pixel8) -> Acc (Matrix Float)
promoteFloat = A.map A.fromIntegral

demoteFloat :: Acc (Matrix Float) -> Acc (Matrix Pixel8)
demoteFloat =  A.map A.truncate

promoteImageF :: Acc (Matrix RGB) -> Acc (Matrix (Float,Float,Float))
promoteImageF img = A.zip3 (A.compute $ promoteFloat r) (A.compute $ promoteFloat g) (A.compute $ promoteFloat b)
    where (r,g,b) = A.unzip3 img

{--
  _   _   _         _                                              
 | | | | (_)  ___  | |_    ___     __ _   _ __    __ _   _ __ ___  
 | |_| | | | / __| | __|  / _ \   / _` | | '__|  / _` | | '_ ` _ \ 
 |  _  | | | \__ \ | |_  | (_) | | (_| | | |    | (_| | | | | | | |
 |_| |_| |_| |___/  \__|  \___/   \__, | |_|     \__,_| |_| |_| |_|
                                  |___/                            
--}


-- gHistogram :: Acc (Matrix (Pixel8, Pixel8, Pixel8))-> (Acc (Vector Int), Acc (Vector Int), Acc (Vector Int))
gHistogram ::  Acc (Matrix (Pixel8, Pixel8, Pixel8)) -> Acc (Vector (Int,Int,Int))
gHistogram arr =
    let (r,g,b) = A.unzip3 arr -- (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)) 
    in A.zip3 (compute $ histogram $ flatten $ promoteInt r) (compute $ histogram $ flatten $ promoteInt g) (compute $ histogram $ flatten $ promoteInt b)

-- Ejemplo sacado de:
-- https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/Data-Array-Accelerate.html
histogram :: Acc (Vector Int) -> Acc (Vector Int)
histogram img =
    let zeros = fill (constant (Z:.256)) 0
        ones  = fill (shape img)         1
        in permute (+) zeros (\ind -> Just_ (I1 (img!ind))) ones

-- {- 
--   ____     ___    __        __
--  | __ )   ( _ )   \ \      / /
--  |  _ \   / _ \/\  \ \ /\ / / 
--  | |_) | | (_>  <   \ V  V /  
--  |____/   \___/\/    \_/\_/   

-- FORMULA PARA SACAR LA LUMINOSIDAD
--     Y' = 0.2989 R + 0.5870 G + 0.1140 B 
--     (https://en.wikipedia.org/wiki/Grayscale)

-- -}
grayScale :: Acc (Matrix (Pixel8,Pixel8,Pixel8)) -> Acc (Matrix Float)
grayScale = A.map (\ pix ->
    let (r,g,b) = unlift pix
        r'      =  A.fromIntegral r / 255
        g'      = A.fromIntegral g / 255
        b'      = A.fromIntegral b / 255
        in r' * 0.2989 + g' * 0.5870 + b' * 0.1140)

-- {-- 

--    ____                               _                     _       _                
--   / ___|   __ _   _   _   ___   ___  (_)   __ _   _ __     | |__   | |  _   _   _ __ 
--  | |  _   / _` | | | | | / __| / __| | |  / _` | | '_ \    | '_ \  | | | | | | | '__|
--  | |_| | | (_| | | |_| | \__ \ \__ \ | | | (_| | | | | |   | |_) | | | | |_| | | |   
--   \____|  \__,_|  \__,_| |___/ |___/ |_|  \__,_| |_| |_|   |_.__/  |_|  \__,_| |_|   

-- --}
-- Ejemplo modificado de accelerate-examples para no utilizar PRAGMAS

-- Modificación del código: https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/Data-Array-Accelerate.html
-- para evitar el uso de la mayoría de los PRAGMAS
type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

convolve5x1 :: (P.Num a, P.Num (Exp a)) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: (P.Num a, P.Num (Exp a)) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

-- Para realizar el blur, una forma en vez de hacer un kernel 5x5 a cada elemento, podemos realizar 
-- 2 pasadas por cada eje usando 
-- blur :: (P.Num a, Elt a, P.Fractional a, P.Num (Exp a)) => Acc (Matrix a) -> Acc (Matrix a)
blur :: Acc (Matrix Float) -> Acc (Matrix Float)
blur = stencil (convolve5x1 gaussian) clamp . stencil (convolve1x5 gaussian) clamp
    where gaussian = P.map A.constant [0.06136,0.24477,0.38774,0.24477,0.06136]

-- TODO: modificar blur para realizar varias iteraciones
blurRGB :: Acc (Matrix RGB) -> Acc (Matrix (Float,Float,Float))
blurRGB img = A.zip3 (A.compute $ blur $ promoteFloat r) (A.compute $ blur $ promoteFloat g) (A.compute $ blur $ promoteFloat b)
    where (r,g,b) = A.unzip3 img



 {-
   __  __                        
 |  \/  |   ___    __ _   _ __  
 | |\/| |  / _ \  / _` | | '_ \ 
 | |  | | |  __/ | (_| | | | | |
 |_|  |_|  \___|  \__,_| |_| |_|
                                 
 -}

meanRGBFilter :: Acc (Matrix (Float,Float,Float)) -> Acc (Matrix (Float,Float,Float))
meanRGBFilter img= A.zip3 (A.compute $ meanChannel r) (A.compute $ meanChannel g) (A.compute $ meanChannel b)
  where (r,g,b) = A.unzip3 img

meanChannel :: Acc (Matrix Float) -> Acc (Matrix Float)
meanChannel img = stencil meanK clamp img
  where meanK ::  Stencil3x3 Float -> Exp Float
        meanK ((a,b,c)
                 ,(d,e,f)
                 ,(g,h,i)) = a/9+b/9+c/9+d/9+e/9+f/9+g/9+h/9+i/9


{-
  ____            _              _ 
 / ___|    ___   | |__     ___  | |
 \___ \   / _ \  | '_ \   / _ \ | |
  ___) | | (_) | | |_) | |  __/ | |
 |____/   \___/  |_.__/   \___| |_|
                                   
El kernel del gradiente X es :
[-1 0 1
 -2 0 2
 -1 0 1]

El kernel del gradiente Y es :
[-1 -2 -1
  0  0  0
  1  2  1]
-}

gradX :: Acc (Matrix Float) -> Acc (Matrix Float)
gradX img = stencil gradient clamp img
  where gradient :: Stencil3x3 Float -> Exp Float
        gradient ((t1,_,t2)
                 ,(l,_,r)
                 ,(b1,_,b2)) = -t1-(2*l)-b1+t2+(2*r)+b2

gradY :: Acc (Matrix Float) -> Acc (Matrix Float)
gradY img = stencil gradient clamp img
  where gradient :: Stencil3x3 Float -> Exp Float
        gradient ((t1,t2,t3)
                 ,(_,_,_)
                 ,(b1,b2,b3)) = -t1-(2*t2)-t3+b1+(2*b2)+b3

sobel :: Acc (Matrix Float) -> Acc (Matrix Float)
sobel img = A.zipWith (\x y  -> A.sqrt (x*x+y*y)) (gradX img) (gradY img)


{-
  _                       _                       
 | |       __ _   _ __   | |   __ _    ___    ___ 
 | |      / _` | | '_ \  | |  / _` |  / __|  / _ \
 | |___  | (_| | | |_) | | | | (_| | | (__  |  __/
 |_____|  \__,_| | .__/  |_|  \__,_|  \___|  \___|
                 |_|                              

-}

-- https://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm

laplace ::  Acc (Matrix Float) -> Acc (Matrix Float)
laplace img = stencil stencilLaplace clamp img
  where stencilLaplace ::  Stencil3x3 Float -> Exp Float
        stencilLaplace ((a,b,c)
                       ,(d,e,f)
                       ,(g,h,i)) = -b-d+(4*e)-f-h

{--
   ____                               _                     ____                                _     _       _                 
  / ___|   __ _   _   _   ___   ___  (_)   __ _   _ __     / ___|   _ __ ___     ___     ___   | |_  | |__   (_)  _ __     __ _ 
 | |  _   / _` | | | | | / __| / __| | |  / _` | | '_ \    \___ \  | '_ ` _ \   / _ \   / _ \  | __| | '_ \  | | | '_ \   / _` |
 | |_| | | (_| | | |_| | \__ \ \__ \ | | | (_| | | | | |    ___) | | | | | | | | (_) | | (_) | | |_  | | | | | | | | | | | (_| |
  \____|  \__,_|  \__,_| |___/ |___/ |_|  \__,_| |_| |_|   |____/  |_| |_| |_|  \___/   \___/   \__| |_| |_| |_| |_| |_|  \__, |
                                                                                                                          |___/ 

--}
-- https://homepages.inf.ed.ac.uk/rbf/HIPR2/gsmooth.htm
--SIGMA =1
gaussianSmoothing :: Acc (Matrix Float) -> Acc (Matrix Float)
gaussianSmoothing img = A.map (/273) $ stencil gaussian clamp img
  where gaussian :: Stencil5x5 Float -> Exp Float
        gaussian ((a1,a2,a3,a4,a5)
                 ,(b1,b2,b3,b4,b5)
                 ,(c1,c2,c3,c4,c5)
                 ,(d1,d2,d3,d4,d5),
                 (e1,e2,e3,e4,e5)) = a1+(4*a2)+(7*a3)+(4*a4)+a5
                                     + (4*b1)+(16*b2)+(26*b3)+(16*b4)+(4*b5)
                                     + (7*c1)+(26*c2)+(41*c3)+(26*c4)+(7*c5)
                                     + (4*d1)+(16*d2)+(26*d3)+(16*d4)+(4*d5)
                                     + e1+(4*e2)+(7*e3)+(4*e4)+e5



test :: IO ()
test  =  do
    putStrLn "Iniciando Test Fichero Accelerate.hs"
    {--Leyendo imágenes --}
    -- img <- readImageAcc "images/saitama.png"
    img <- readImageAcc "data/images/lena_color.png"

    {--Creación Histograma--}
    -- let (r,g,b) = gHistogram (use img)
    -- El comando run nos trae devuelta los datos de la GPU
    -- print (run r, run g , run b)

    {--Blanco y Negro--}
    let gr = PTX.run $ grayScale (use img)
    savePngImage "outputgreyacc.png" (ImageYF $ greyToJcy gr)

    {--Filtros--}

    -- let blr = run $ blur $ blur $ blur (use gr)
    -- savePngImage "axaa.png" (ImageYF $ greyToJcy blr)

    -- let sobel = run $ magnitude (use gr)
    -- savePngImage "sobelAcc.png" (ImageYF $ greyToJcy sobel)

    -- let imagelaplace =  run $ laplace (use gr)
    -- savePngImage "laplaceAcc.png" (ImageYF $ greyToJcy imagelaplace)

    let gausssmouth =  run $ gaussianSmoothing (use gr)
    savePngImage "xxxxxxxx.png" (ImageYF $ greyToJcy gausssmouth)


    putStrLn "Fin del Test fichero Acelerate.hs"