module Main where

import JuicyRepa as Rtest
import JuicyAccelerate as Atest
import Repa as Rx
import Accelerate as Ax
import Benchmark as B

-- MAIN 
main :: IO ()
main =  do
    -- Rx.test
    -- Ax.test
    B.test
    putStrLn  "Fin del Main"