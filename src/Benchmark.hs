-- |
-- Module:      : Benchmark
-- Copyright    : [2022] Kenny Jesús Flores Huamán
-- License      : BSD3
--
-- Maintainer   : Kenny Jesús Flores Huamán <kennyjesus@pm.me>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)

-- Módulo para revisar las resoluciones 

module Benchmark where

-- Importamos modulos
-- Módulo para benchmarks
import Criterion.Main
import Control.Monad
import Prelude as P

-- Repa
import qualified Data.Array.Repa as R
-- Accelerate
import qualified Data.Array.Accelerate as A
-- import qualified Data.Array.Accelerate.LLVM.PTX as PTX
-- import qualified Data.Array.Accelerate.LLVM.Native as Native
import Backend as B

-- Algoritmos Realizados
import qualified Repa as R
import qualified Accelerate as A

-- Lectura Juicy
import JuicyAccelerate
import JuicyRepa
import Accelerate
import Codec.Picture

-- Función de prueba para testear Criterion: https://github.com/haskell/criterion/blob/master/examples/Fibber.hs
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)


-- Ejemplo de como se realizarían los test en Repa utilizando criterion
testfib = defaultMain [
  bgroup "fib" [ bench "Ejemplo fib(1)"  $ whnf fib 1
               , bench "Ejemplo fib (5)"  $ whnf fib 5
               , bench "Ejemplo fib(11)" $ whnf fib 11
               ]
  ]


test :: IO ()
test = do
    putStrLn "Inicio de los test"
    -- Imagen de prueba
    imgRepa <- readImageIntoRepa "data/images/lena_color.png"
    imgAcc <- readImageAcc "data/images/lena_color.png"
    -- Histogram 
    let hstRepaV1 = R.generateHistogramsV1 <$> mapM R.promoteInt imgRepa
    let hstRepaV2 = R.generateHistogramsV2 <$> mapM R.promoteInt imgRepa
    let hstRepaV3 = R.generateHistogramsV3 <$> mapM R.promoteInt imgRepa
    -- Black and white
    let bwRepaV1 = R.toGrayScaleV1 imgRepa
    let bwRepaV2 = R.toGrayScaleV2 imgRepa

    -- Gaussian Blur
    let gaussRepaV1 =  mapM (R.promote >=> R.blurV1 1) imgRepa
    let gaussRepaV2 = mapM (R.promote >=> R.blurV2 1) imgRepa

    defaultMain [
                  bgroup "Histogram" [ bench "Repa V1: Sequencial"  $ nfIO hstRepaV1,
                                       bench "Repa V2: Cada canal en paralelo"  $ nfIO hstRepaV2,
                                       bench "Repa V3: Cada canal y fila en paralelo"  $ nfIO hstRepaV3,
                                       bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) gHistogram) imgAcc,
                                       bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) gHistogram) imgAcc,
                                       bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                       bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) gHistogram) imgAcc
                                       ],

                  bgroup "GreyScale" [ bench "Repa V1: Uso de zipWith"  $ nfIO bwRepaV1,
                                       bench "Repa v2: Uso de zip3" $ nfIO bwRepaV2,
                                       bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) A.grayScale) imgAcc,
                                       bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) A.grayScale) imgAcc,
                                       bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                       bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) A.grayScale) imgAcc
                                       ],

                  bgroup "Guassian Blur" [ bench "Repa V1: Uso de 2 Kernels 1x5 y 5x1"  $ nfIO gaussRepaV1,
                                           bench "Repa v2: Uso de 1 kernel 5x5" $ nfIO gaussRepaV2,
                                           bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) A.blurRGB) imgAcc,
                                           bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) A.blurRGB) imgAcc,
                                           bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                           bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) A.blurRGB) imgAcc
                                        ],

                  bgroup "Mean" [ bench "Repa : Mean Filter"  $ whnf (mapM $ R.promote >=> R.meanF) imgRepa,
                                  bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) (A.meanRGBFilter . promoteImageF)) imgAcc,
                                  bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) (A.meanRGBFilter . promoteImageF)) imgAcc,
                                  bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                  bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) (A.meanRGBFilter . promoteImageF)) imgAcc
                                 ],

                  bgroup "Sobel" [ bench "Repa: Paralelismo CPU"  $ whnfIO (R.sobel <$> R.toGrayScaleV1 imgRepa),
                                   bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) (A.sobel . A.grayScale)) imgAcc,
                                   bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) (A.sobel . A.grayScale)) imgAcc,
                                   bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                   bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) (A.sobel . A.grayScale)) imgAcc
                                 ],
----------------------------------------------------------------------------------------------------------------
                  bgroup "Laplace" [ bench "Repa: Paralelismo CPU"  $ whnfIO (R.laplace <$> R.toGrayScaleV1 imgRepa),
                                   bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) (A.laplace . A.grayScale)) imgAcc,
                                   bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) (A.laplace . A.grayScale)) imgAcc,
                                   bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                   bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) (A.laplace . A.grayScale)) imgAcc
                                   ]
                ]

    putStrLn "Fin de los test"



test2 :: IO ()
test2 = do

  imgRepa <- readImageIntoRepa "data/images/960x540.jpg"
  imgAcc <- readImageAcc "data/images/960x540.jpg"

  -- Gauss blur (RGB)
  -- Gaussian Blur
  let gaussRepaV1RGB =  mapM (R.promote >=> R.blurV1 1) imgRepa
  let gaussRepaV2RGB = mapM (R.promote >=> R.blurV2 1) imgRepa

  defaultMain [

                  bgroup "Gaussian Blur (RGB) Uso kernel 1x5 y 5x1" [ bench "Repa V1: Uso de 2 Kernels 1x5 y 5x1"  $ nfIO gaussRepaV1RGB,
                                           bench "AccelerateV1 : Interprete" $ whnf (B.run1 (selectBackend 0) A.blurRGB) imgAcc,
                                           bench "AccelerateV1 : CPU" $ whnf (B.run1 (selectBackend 1) A.blurRGB) imgAcc,
                                           bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                           bench "AccelerateV1 : GPU" $ whnf (B.run1 (selectBackend 2) A.blurRGB) imgAcc

                  ],

                  bgroup "Gaussian Blur (RGB) Uso kernel 5x5 " [ bench "Repa v2: Uso de 1 kernel 5x5" $ nfIO gaussRepaV2RGB,
                                           bench "AccelerateV2 : Interprete" $ whnf (B.run1 (selectBackend 0) (A.gaussianSmoothingRGB . promoteImageF)) imgAcc,
                                           bench "AccelerateV2 : CPU" $ whnf (B.run1 (selectBackend 1) (A.gaussianSmoothingRGB . promoteImageF)) imgAcc,
                                           bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                           bench "AccelerateV2 : GPU" $ whnf (B.run1 (selectBackend 2) (A.gaussianSmoothingRGB . promoteImageF)) imgAcc

                  ],

                  bgroup "Mean (Optimizado)" [ bench "Repa : Mean Filter"  $ whnf (mapM $ R.promote >=> R.meanF) imgRepa,
                                  bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) (A.meanRGBFilter . promoteImageF)) imgAcc,
                                  bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) (A.meanRGBFilter . promoteImageF)) imgAcc,
                                  bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                  bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) (A.meanRGBFilter . promoteImageF)) imgAcc
                                 ]

              
                ]


  putStrLn "Hehe"