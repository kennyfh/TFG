{-# LANGUAGE FlexibleContexts #-}
module Accelerate where

import JuicyAccelerate
import Codec.Picture
import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Interpreter as I -- TODO: modificar esto por el interprete de CUDA 
import Data.Array.Accelerate.LLVM.PTX
import Prelude as P 
import Control.Monad ((>=>))

promoteInt :: Acc (Matrix Pixel8) -> Acc (Matrix Int)
promoteInt = A.map A.fromIntegral

demoteInt :: Acc (Matrix Int) -> Acc (Matrix Pixel8)
demoteInt =  A.map A.fromIntegral

promoteFloat :: Acc (Matrix Pixel8) -> Acc (Matrix Float)
promoteFloat = A.map A.fromIntegral

demoteFloat :: Acc (Matrix Float) -> Acc (Matrix Pixel8)
demoteFloat =  A.map A.truncate


{--
  _   _   _         _                                              
 | | | | (_)  ___  | |_    ___     __ _   _ __    __ _   _ __ ___  
 | |_| | | | / __| | __|  / _ \   / _` | | '__|  / _` | | '_ ` _ \ 
 |  _  | | | \__ \ | |_  | (_) | | (_| | | |    | (_| | | | | | | |
 |_| |_| |_| |___/  \__|  \___/   \__, | |_|     \__,_| |_| |_| |_|
                                  |___/                            
--}


gHistogram :: Acc (Matrix (Pixel8, Pixel8, Pixel8))-> (Acc (Vector Int), Acc (Vector Int), Acc (Vector Int))
gHistogram arr =
    let (r,g,b) = A.unzip3 arr -- (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)) 
    in (compute $ histogram $ flatten $ promoteInt r, compute $ histogram $ flatten $ promoteInt g,compute $ histogram $ flatten $ promoteInt b)

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
blur :: (P.Num a, Elt a, P.Fractional a, P.Num (Exp a)) => Acc (Matrix a) -> Acc (Matrix a)
blur = stencil (convolve5x1 gaussian) clamp . stencil (convolve1x5 gaussian) clamp
    where gaussian = P.map A.constant [0.06136,0.24477,0.38774,0.24477,0.06136]

-- TODO: modificar blur para realizar varias iteraciones

 {-
   __  __                        
 |  \/  |   ___    __ _   _ __  
 | |\/| |  / _ \  / _` | | '_ \ 
 | |  | | |  __/ | (_| | | | | |
 |_|  |_|  \___|  \__,_| |_| |_|
                                 
 -}

meanF :: Acc (Matrix Float) -> Acc (Matrix Float)
meanF img = stencil meanK clamp img
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

magnitude :: Acc (Matrix Float) -> Acc (Matrix Float)
magnitude img = A.zipWith (\x y  -> A.sqrt (x*x+y*y)) (gradX img) (gradY img)


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
    let gr = run $ grayScale (use img)
    savePngImage "saitamagrayscale.png" (ImageYF $ greyToJcy gr)

    {--Filtros--}

    -- let blr = run $ blur $ blur $ blur (use gr)
    -- savePngImage "axaa.png" (ImageYF $ greyToJcy blr)

    let sobel = run $ magnitude (use gr)
    savePngImage "sobelAcc.png" (ImageYF $ greyToJcy sobel)

    putStrLn "Fin del Test fichero Acelerate.hs"