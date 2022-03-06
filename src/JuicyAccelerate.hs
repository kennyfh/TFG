module JuicyAccelerate where


{-- MODULOS IMPORTADOS-}

import Data.Array.Accelerate.IO.Codec.Picture
-- import Data.Ar
import Juicy
import Data.Array.Accelerate hiding (fromIntegral)
import Prelude hiding (map)
import Data.Array.Accelerate.Data.Colour.RGB


{-- TIPOS DE DATOS UTILIZADOS--}

{-- FUNCIONES--}

-- readImageIntoAcc :: FilePath -> IO (Array DIM2 PixelRGB8)
readImageIntoAcc path =  unpackRGB $ arrayOfImage <$> readImageJuicy path





{-- TEST --}
test :: IO ()
test  = do
    img <- readImageIntoAcc "saitama.png"


    putStrLn "Hello World"