module Main where

import Lib
import Codec.Picture

-- MAIN 
main :: IO ()
main =  do
    putStrLn "Bienvenido, introduzca la ruta del video para resumir"
    path <- getLine
    ls <- getAllFrames path --  IO [(Double, Image PixelRGB8)]
    -- putStrLn $ show $ fst $ last ls
    -- savePngImage "prueba.png" (ImageRGB8 $ snd $ last ls)
    -- putStrLn "Ha llegado hasta aquí"
    saveVideo ls "nuevoultimo.mp4"
    putStrLn  "Fin del Main"


