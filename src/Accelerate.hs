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


-- gHistogram :: Acc (Matrix (Pixel8, Pixel8, Pixel8))-> (Acc (Vector Int), Acc (Vector Int), Acc (Vector Int))
-- gHistogram arr =
--     let (r,g,b) = A.unzip3 arr -- (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)) 
--     in (compute $ histogram $ flatten $ promoteInt r, compute $ histogram $ flatten $ promoteInt g,compute $ histogram $ flatten $ promoteInt b)

-- histogram :: Acc (Vector Int) -> Acc (Vector Int)
-- histogram img =
--     let zeros = fill (constant (Z:.256)) 0
--         ones  = fill (shape img)         1
--         in permute (+) zeros (\ind -> Just_ (I1 (img!ind))) ones


-- promoteInt :: Acc (Matrix Pixel8) -> Acc (Matrix Int)
-- promoteInt = A.map A.fromIntegral


-- {- 
--   ____     ___    __        __
--  | __ )   ( _ )   \ \      / /
--  |  _ \   / _ \/\  \ \ /\ / / 
--  | |_) | | (_>  <   \ V  V /  
--  |____/   \___/\/    \_/\_/   
                              
-- FORMULA PARA SACAR LA LUMINOSIDAD
--     Y' = 0.2989 R + 0.5870 G + 0.1140 B 
--     (https://en.wikipedia.org/wiki/Grayscale)

-- -}

-- grayScale :: Acc (Matrix (Pixel8,Pixel8,Pixel8)) -> Acc (Matrix Float)
-- grayScale = A.map (\ pix ->
--     let (r,g,b) = unlift pix
--         r'      =  A.fromIntegral r / 255
--         g'      = A.fromIntegral g / 255
--         b'      = A.fromIntegral b / 255
--         in r' * 0.3 + g' * 0.59 + b' * 0.11)

-- {-- 

--    ____                               _                     _       _                
--   / ___|   __ _   _   _   ___   ___  (_)   __ _   _ __     | |__   | |  _   _   _ __ 
--  | |  _   / _` | | | | | / __| / __| | |  / _` | | '_ \    | '_ \  | | | | | | | '__|
--  | |_| | | (_| | | |_| | \__ \ \__ \ | | | (_| | | | | |   | |_) | | | | |_| | | |   
--   \____|  \__,_|  \__,_| |___/ |___/ |_|  \__,_| |_| |_|   |_.__/  |_|  \__,_| |_|   
                                                                                     
-- --}



test :: IO ()
test  =  do
    putStrLn "Iniciando Test Fichero Accelerate.hs"
    {--Leyendo imágenes --}
    -- img <- readImageAcc "saitama.png"

    {--Creación Histograma--}
    -- let (r,g,b) = gHistogram (use img)
    -- print (run r, run g , run b)

    {--Blanco y Negro--}
    -- let asd = run $ grayScale (use img)
    -- savePngImage "ejemplo000000.png" (ImageYF $ toJcyBandW asd)

    {--Filtros--}

    putStrLn "ASD"