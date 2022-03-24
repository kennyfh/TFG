module JuicyAccelerate where

import Juicy
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Codec.Picture
import Prelude as P
import Data.Array.Accelerate.Interpreter (run)
import Data.Array.Accelerate.IO.Codec.Picture.Types as PT ()

readImg :: FilePath -> IO (Matrix PixelRGB8)
readImg p  = arrayOfImage <$> readImageJuicy p


-- func :: Acc (Matrix PixelRGB8) -> 

test :: IO ()
test =  do
    img <- readImg "axxa.png"
    -- let x =  indexArray img (Z :. 30 :.330)
    print $ run $ A.map (\ (PixelRGB8_ r g b) -> ) (Acc (Array sh a))
    -- let (PixelRGB8 r g b) = x
    -- print r
    
    print x
    
    putStrLn "Fin de la prueba"