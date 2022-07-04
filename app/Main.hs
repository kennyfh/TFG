module Main where

import JuicyRepa as Rtest
import JuicyAccelerate as Atest
import Repa as Rx
import Accelerate as Ax
import Benchmark as B

-- MAIN 
main :: IO ()
main =  do
    -- B.vidGaussRepa
    -- B.vidGaussAcc
    -- Rx.test
    -- Ax.test
    -- B.testGrayScale
    -- B.testSobel
    -- B.testHistograma
    -- B.testLaplace
    -- Rx.test
    -- B.testB
    B.scalarPTest

    putStrLn  "Fin del Main"