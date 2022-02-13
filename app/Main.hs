module Main where

import Video
import Image
import Repa as R
-- MAIN 
main :: IO ()
main =  do
    R.test 
    -- testImage
    -- putStrLn "Bienvenido, introduzca la ruta del video para resumir"
    -- path <- getLine
    -- ls <- getAllFrames path -- [(Double, Image PixelRGB8)] <- IO [(Double, Image PixelRGB8)]
    -- let xs  =  map fst ls
    -- -- putStrLn $ show xs
    -- saveVideo ls "nuevoultimo.mp4"
    putStrLn  "Fin del Main"
