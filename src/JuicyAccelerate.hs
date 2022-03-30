module JuicyAccelerate where

-- import Juicy
-- import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.IO.Codec.Picture
-- import Prelude as P
-- import Data.Array.Accelerate.Interpreter (run)
-- import Data.Array.Accelerate.IO.Codec.Picture.Types as PT ()

-- readImg :: FilePath -> IO (Matrix PixelRGB8)
-- readImg p  = arrayOfImage <$> readImageJuicy p


-- -- func :: Acc (Matrix PixelRGB8) -> 

-- test :: IO ()
-- test =  do
--     img <- readImg "axxa.png"
--     -- let x =  indexArray img (Z :. 30 :.330)
--     print $ run $ A.map (\ (PixelRGB8_ r g b) -> ) (Acc (Array sh a))
--     -- let (PixelRGB8 r g b) = x
--     -- print r
    
--     print x
    
--     putStrLn "Fin de la prueba"


{-- MODULOS IMPORTADOS-}

import Data.Array.Accelerate.IO.Codec.Picture
import Data.Array.Accelerate.IO.Codec.Picture.Types
import Juicy
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Prelude hiding (map)
import Codec.Picture
-- import JuicyRepa as JR
-- import Data.Array.Repa.Repr.Accelerate

{-- TIPOS DE DATOS UTILIZADOS--}
type RGB = (Pixel8,Pixel8,Pixel8)
{-- FUNCIONES--}

readImageAcc :: FilePath -> IO (Matrix RGB)
readImageAcc path =  toArr <$> readImageJuicy path

pp :: Matrix RGB -> Matrix PixelRGB8 
pp arr =  run $ A.map (\p -> let (r,g,b) =  unlift p in PixelRGB8_ r g b) (use arr)


toArr :: Image PixelRGB8 -> Matrix RGB -- (Exp WORD8,Exp word8,Exp word8)
toArr img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData} =
    fromFunction (Z :. imageHeight :. imageWidth) (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img y x
       in (r, g, b))



toJcyBandW :: Matrix Float -> Image PixelF
toJcyBandW = imageOfArray

-- Exp

{-- TEST --}
test :: IO ()
test  = do
    img <- readImageAcc "saitama.png"
    let xs = pp img
    let xs2 = imageOfArray xs
    savePngImage "locora.png" (ImageRGB8 xs2)
    -- print img
    let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
    print $ run $ A.map (+1) (use arr)
    putStrLn "Hello World"



