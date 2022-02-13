module Image where

import Codec.Picture
import Data.Array.Repa as R
import Codec.Picture.Types
import Control.Monad (forM)



testImage :: IO ()
testImage = do
    -- test <- readImageJuicy "saitama.png"
    -- let asd = channeljuicyToRepa $ extractChannel 0 test
    -- print asd
    asd <- readImageIntoRepa "saitama.png"
    -- print $ asd !! 2
    -- let xs = extractRedPlane test
    -- savePngImage "saitamared.png" (ImageY8 xs)
    putStrLn "asdasdasd"

{- Tipos de datos que podemos utilizar-}
type Channel a = Array U DIM2 a
type ImgRGB = [Channel Int]
type ImgA = Channel Float

-- Función genérica que nos lee las imágenes en RGB8 en formato
-- JuicyPixels
readImageJuicy :: FilePath -> IO (Image PixelRGB8)
readImageJuicy path = do
    img <- readImage path
    case img of
        Left e -> fail e
        Right img -> return $ convertRGB8 img

{-Función que nos permite extraer los canales RGB-}
extractChannel :: Int-> Image PixelRGB8 -> Image Pixel8
extractChannel 0 img= extractComponent PlaneRed img
extractChannel 1 img = extractComponent PlaneGreen img
extractChannel 2 img = extractComponent PlaneBlue img
extractChannel _ img = undefined --no tenemos más canales

channeljuicyToRepa :: Image Pixel8-> IO (Channel Int)
channeljuicyToRepa img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData}=
    R.computeP $ R.fromFunction (Z :. imageWidth :. imageHeight) (\(Z :. x :. y)-> fromIntegral $ pixelAt img x y)

listOfBands :: Image PixelRGB8 -> [Image Pixel8]
listOfBands img = [extractChannel i img | i<-[0..2]]

readImageIntoRepa :: FilePath -> IO ImgRGB
readImageIntoRepa path = do
    img <- readImageJuicy path
    let xs = listOfBands img
    forM xs channeljuicyToRepa

repaToJuicy :: ImgRGB -> Image PixelRGB8 
repaToJuicy img = generateImage func w h
    where [r,g,b] = img
          Z :. w :. h = R.extent r
          func x y = 
              let r' = fromIntegral $ r ! (Z :. x :. y)
                  g' = fromIntegral $  g ! (Z :. x :. y)
                  b' = fromIntegral $ b ! (Z :. x :. y)
               in PixelRGB8 r' g' b'