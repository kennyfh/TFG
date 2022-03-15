module Accelerate where

import JuicyAccelerate
import Codec.Picture
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

{--
  _   _   _         _                                              
 | | | | (_)  ___  | |_    ___     __ _   _ __    __ _   _ __ ___  
 | |_| | | | / __| | __|  / _ \   / _` | | '__|  / _` | | '_ ` _ \ 
 |  _  | | | \__ \ | |_  | (_) | | (_| | | |    | (_| | | | | | | |
 |_| |_| |_| |___/  \__|  \___/   \__, | |_|     \__,_| |_| |_| |_|
                                  |___/                            
--}


gHistogram :: Acc (Array DIM2 (Pixel8, Pixel8, Pixel8))-> (Acc (Vector Int), Acc (Vector Int), Acc (Vector Int))
gHistogram arr =
    let (r,g,b) = A.unzip3 arr -- (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)) 
    in (histogram $ flatten $ promoteInt r, histogram $ flatten $ promoteInt g, histogram $ flatten $ promoteInt b)

histogram :: Acc (Vector Int) -> Acc (Vector Int)
histogram xs =
    let zeros = fill (constant (Z:.10)) 0
        ones  = fill (shape xs)         1
        in permute (+) zeros (\ix -> Just_ (I1 (xs!ix))) ones


promoteInt :: Acc (Array DIM2 Pixel8) -> Acc (Array DIM2 Int)
promoteInt = A.map A.fromIntegral

test :: IO ()
test  =  do
    putStrLn "Iniciando Test Fichero Accelerate.hs"
    img <- readImageAcc "saitama.png"
    -- print img
    print $ gHistogram (use img)
    

    putStrLn "ASD"