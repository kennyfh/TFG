module Main where

import Lib
import Codec.Picture

-- MAIN 
main :: IO ()
main =  do
    putStrLn "Bienvenido, introduzca la ruta del video para resumir"
    path <- getLine
    ls <- getAllFrames path -- [(Double, Image PixelRGB8)] <- IO [(Double, Image PixelRGB8)]
    let xs  =  map fst ls
    -- putStrLn $ show xs
    saveVideo ls "nuevoultimo.mp4"
    putStrLn  "Fin del Main"


