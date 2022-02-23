module JuicyAccelerate where


{-- MODULOS IMPORTADOS-}

import Data.Array.Accelerate.IO.Codec.Picture
import Juicy
import Data.Array.Accelerate
{-- TIPOS DE DATOS UTILIZADOS--}


{-- FUNCIONES--}
readImageIntoAcc :: FilePath -> IO (Array DIM2 PixelRGB8)
readImageIntoAcc path = arrayOfImage <$> readImageJuicy path


{-- TEST --}
test :: IO ()
test  = do 
    img <- readImageIntoAcc "saitama.png"
     
    putStrLn "Hello World"