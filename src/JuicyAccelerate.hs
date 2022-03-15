module JuicyAccelerate where


{-- MODULOS IMPORTADOS-}

import Data.Array.Accelerate.IO.Codec.Picture
import Juicy
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Prelude hiding (map)
import Codec.Picture
import JuicyRepa as JR
import Data.Array.Repa.Repr.Accelerate

{-- TIPOS DE DATOS UTILIZADOS--}
type RGB = (Pixel8,Pixel8,Pixel8)
{-- FUNCIONES--}

readImageAcc :: FilePath -> IO (Array DIM2 RGB)
readImageAcc path =   toArr <$> readImageJuicy path 


toArr :: Image PixelRGB8 -> Array DIM2 (Pixel8, Pixel8, Pixel8)
toArr img = fromFunction (Z :. width :. height) (\(Z :. x :. y) -> let (PixelRGB8 r g b) = pixelAt img x y in (r, g, b))
            where width = imageWidth img
                  height = imageHeight img


--- Usar la librería Accelerate-io-repa para cambiarlo a repa
-- y posteriormente 
-- convertArrToImg :: Array DIM2 (Pixel8,Pixel8,Pixel8) -> Image PixelRGB8
-- convertArrToImg arr =
--     let (r,g,b) = A.unzip3 arr
--         in JR.repaToJuicy [toRepa r, toRepa g, toRepa b]


{-- TEST --}
test :: IO ()
test  = do
    img <- readImageAcc "saitama.png"
    let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
    print $ run $ A.map (+1) (use arr)
    putStrLn "Hello World"



