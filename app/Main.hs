module Main where

import JuicyRepa as Rtest
import JuicyAccelerate as Atest
import Repa as Rx
import Accelerate as Ax
import PruebasRandom as Rn
-- MAIN 
main :: IO ()
main =  do
    -- Rn.test
    -- Rx.test 
    Ax.test
    -- testImage
    -- putStrLn "Bienvenido, introduzca la ruta del video para resumir"
    -- path <- getLine
    -- ls <- getAllFrames path -- [(Double, Image PixelRGB8)] <- IO [(Double, Image PixelRGB8)]
    -- let xs  =  map fst ls
    -- -- putStrLn $ show xs
    -- saveVideo ls "nuevoultimo.mp4"
    putStrLn  "Fin del Main"
