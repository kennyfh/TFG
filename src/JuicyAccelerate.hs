module JuicyAccelerate where


{-- MODULOS IMPORTADOS-}

import Data.Array.Accelerate.IO.Codec.Picture
import Juicy
import Data.Array.Accelerate hiding (fromIntegral)
import Prelude hiding (map)

{-- TIPOS DE DATOS UTILIZADOS--}

type RGB =  (Int,Int,Int)

{-- FUNCIONES--}

readImageIntoAcc :: FilePath -> IO (Array DIM2 PixelRGB8)
readImageIntoAcc path = arrayOfImage <$> readImageJuicy path

promote :: PixelRGB8 -> (Int,Int,Int)
promote (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

{--
NOTA IMPORTANTE, podemos convertir el array de dim en un triple WORD8 pudiendo separarlos en canales utilizando
la función unzip3

r, g, b) <- liftM (either (error . show) R.unzip3) readImageFromBMP "in.bmp"
[r’, g’, b’] <- mapM (applyStencil simpleStencil) [r, g, b]
writeImageToBMP "out.bmp" (U.zip3 r’ g’ b’)


-}


{-- TEST --}
test :: IO ()
test  = do
    img <- readImageIntoAcc "saitama.png"


    putStrLn "Hello World"