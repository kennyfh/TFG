module Repa where

import Data.Array.Repa as R
import Image
import Data.List as L ( foldl')
import Data.Sequence as S
import Codec.Picture

test :: IO ()
test = do
    -- Prueba Histogramas
    img <- readImageIntoRepa "saitama.png"
    let asd = generateHistograms img
    -- print asd
    alpha <- toGrayScale img
    -- print alpha
    let du = ImageRGB8 $ repaToJuicy img
    savePngImage "saitamanew.png" du
    putStrLn "Fin de test"


{-Tipos de Datos-}
type Histogram = Seq Int
type Histograms = [Histogram]

{-HISTOGRAM -}
generateHistograms :: ImgRGB -> Histograms
generateHistograms [] = []
generateHistograms (x:xs) = generateHistogram x : generateHistograms xs

generateHistogram :: Channel Int -> Histogram
generateHistogram c =
    let (Z :. w :. h)  = R.extent c
        zero = S.replicate 256 0
    in L.foldl' (\hst (x,y) ->
        let val = unsafeIndex c (Z :. x :. y)
            cont = hst `S.index` val
        in S.update val (cont+1) hst)
        zero
        [(x,y) | x<-[0..w-1],y<-[0..h-1]]

-- blanco y negro
{- FORMULA PARA SACAR LA LUMINOSIDAD
    Y' = 0.2989 R + 0.5870 G + 0.1140 B 
    (https://en.wikipedia.org/wiki/Grayscale)
-}

toGrayScale :: ImgRGB -> IO ImgA
toGrayScale img = R.computeP $ R.zipWith (+) b (R.zipWith (+) r g)
    where r = generateComponentB 0 (head img)
          g = generateComponentB 1 (img !! 1)
          b = generateComponentB 2 (last img)

generateComponentB :: Int -> Channel Int -> Array D DIM2 Float
generateComponentB ind = R.map (\v -> (fromIntegral (fromIntegral v ::Int) / 255)*val)
    where val = [0.2989,0.5870,0.1140] !! ind


{---}