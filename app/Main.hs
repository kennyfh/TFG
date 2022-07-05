-- |
-- Module:      : Main
-- Copyright    : [2022] Kenny Jesús Flores Huamán
-- License      : BSD3
--
-- Maintainer   : Kenny Jesús Flores Huamán <kennyjesus@pm.me>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)

module Main where

import JuicyRepa as Rtest
import JuicyAccelerate as Atest
import Repa as Rx
import Accelerate as Ax
import Benchmark as B

-- MAIN 
main :: IO ()
main =  do
    --Mean 
    --B.vidMeanAcc
    --B.vidMeanRepa

    --Gauss
    -- B.vidGaussAcc
    -- B.vidGaussRepa

    -- B.testGrayScale
    -- B.testSobel
    -- B.testHistograma
    -- B.testLaplace

    putStrLn  "Fin del Main"

testHaskell :: IO ()
testHaskell = do
    let xs = B.scalarp [1..10000000] [1..10000000] 
    print xs

testRepa :: IO ()
testRepa = Rx.testScalarRepa

testAcc :: IO ()
testAcc = Ax.testScalarAcc


-------------------FUNCIONES PARA STACK GHCI---------------------------------

--- Mean Acc
testVidMeanAcc :: IO ()
testVidMeanAcc = B.vidMeanAcc

---Mean Repa
testVidMeanRepa :: IO ()
testVidMeanRepa = B.vidMeanRepa


--Gaussian Acc
testVidGaussAcc :: IO ()
testVidGaussAcc = B.vidGaussAcc

-- Gaussian RepatestVidMeanRepa :: IO 
testVidGaussRepa :: IO ()
testVidGaussRepa = B.vidGaussRepa

