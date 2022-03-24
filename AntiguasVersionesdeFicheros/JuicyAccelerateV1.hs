module JuicyAccelerateV1 where


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

readImageAcc :: FilePath -> IO (Matrix PixelRGB8_)
readImageAcc path =   arrayOfImage <$> readImageJuicy path

convertLALA :: Matrix PixelRGB8 -> Matrix RGB
convertLALA arr = run $ A.map (A.lift3 (+2)) (use arr)


-- toArr :: Image PixelRGB8 -> Matrix RGB
-- toArr img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData} =
--     fromFunction (Z :. imageWidth :. imageHeight) (\(Z :. x :. y) ->
--        let (PixelRGB8 r g b) = pixelAt img x y
--        in (r, g, b))

toJcyBandW :: Matrix Float -> Image PixelF
toJcyBandW = imageOfArray



{-- TEST --}
test :: IO ()
test  = do
    img <- readImageAcc "saitama.png"
    print img
    let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
    print $ run $ A.map (+1) (use arr)
    putStrLn "Hello World"



