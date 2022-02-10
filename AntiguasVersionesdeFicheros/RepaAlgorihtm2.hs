module RepaAlgorihtm2 where

import Codec.Picture as P
import Data.Array.Repa as R
import Data.Vector.Fixed as VF

prueba3 ::IO()
prueba3 = do
    let a = VF.mk3 7 7 7 :: (Int,Int,Int)
    -- let b  = VF.mk3 2 2 2 :: (Int,Int,Int)
    -- print $ VF.sum $ VF.zipWith (*) a b
    -- -- print z
    x <- readImageR "saitama.png"
    let asd = readImgRepa x
    let xx = asd R.! (Z :. 400 :. 400)
    print $ VF.map (*2) xx
    putStrLn "Hy"

-- ([r,r,r,r],[g,g,g,g],[b,b,b,b,b]) IDEA

-- Estructuras de datos a usar
type RGB = (Int,Int,Int)
type Img v = Array U DIM2 v

fileToArray :: FilePath -> IO (Img RGB)
fileToArray p = readImgRepa <$> readImageR p

-- Lee la imagen y lo convierte en una imagen JuicyPixels
readImageR :: FilePath -> IO (Image PixelRGB8)
readImageR path = do
    res <- P.readImage path
    case res of
        Left s -> fail s
        Right img -> return $ convertRGB8 img

-- readImgRepa :: Image PixelRGB8 -> Img 
readImgRepa :: Image PixelRGB8 -> Img RGB
readImgRepa img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData} =
    R.computeS $ R.fromFunction (Z :. imageWidth :. imageHeight) (\(Z :. x :. y) ->
        let (PixelRGB8 r g b) = pixelAt img x y
        in VF.mk3 (fromIntegral r) (fromIntegral g) (fromIntegral b) ::(Int,Int,Int))

