module Image where

import Codec.Picture
import Data.Array.Repa as R

{- Tipos de datos que podemos utilizar-}
type Channel a = Array U DIM1 a
type ImgRGB = (Channel Int, Channel Int,Channel Int)
type ImgA = Channel Float 

-- Función genérica que nos lee las imágenes en RGB8 en formato
-- JuicyPixels
readImageJuicy :: FilePath -> IO (Image PixelRGB8 )
readImageJuicy path = do
    img <- readImage path
    case img of
        Left e -> fail e
        Right img -> return $ convertRGB8 img


JuicyToRepa :: Image PixelRGB8 -> ImgR 