module Main where

import JuicyRepa as Rtest
import JuicyAccelerate as Atest
import Repa as Rx
import Accelerate as Ax
import Kmeans as K
import VideoSummarization as VS

-- MAIN 
main :: IO ()
main =  do
    Rx.test
    putStrLn  "Fin del Main"