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
import qualified Data.Array.Accelerate.LLVM.PTX as GPU 

-- Algoritmos Realizados
import qualified Repa as R
import qualified Accelerate as A

-- Lectura Juicy
import JuicyAccelerate
import JuicyRepa

-- Función de prueba: https://github.com/haskell/criterion/blob/master/examples/Fibber.hs
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)


-- Ejemplo de como se realizarían los test en Repa        
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
    --zip3 :: (Shape sh, Elt a, Elt b, Elt c) => Acc (Array sh a) -> Acc (Array sh b) -> Acc (Array sh c) -> Acc (Array sh (a, b, c)) 
    -- let hstAcc = GPU.run $ A.zip3 $ gHistogram (use imgAcc)
    
    -- Black and white
    let bwRepaV1 = R.toGrayScaleV1 imgRepa
    let bwRepaV2 = R.toGrayScaleV2 imgRepa

    -- Gaussian Blur
    let gaussRepaV1 =  mapM (R.promote >=> R.blurV1 4) imgRepa
    let gaussRepaV2 = mapM (R.promote >=> R.blurV2 4) imgRepa

    -- Sobel 
    imgGrey <- bwRepaV1
    let sobelRepa = R.sobel imgGrey

    defaultMain [ 
                  -- bgroup "Histogram" [ bench "Repa V1: Sequencial"  $ nfIO hstRepaV1,
                  --                      bench "Repa V2: Cada canal en paralelo"  $ nfIO hstRepaV2,
                  --                      bench "Repa V3: Cada canal y fila en paralelo"  $ nfIO hstRepaV3
                  --                      -- ,bench "Accelerate" $ nf hstAcc
                  --                      ],

                  -- bgroup "GreyScale" [ bench "Repa V1: Uso de zipWith"  $ nfIO bwRepaV1,
                  --                      bench "Repa v2: Uso de zip3" $ nfIO bwRepaV2
                  --                      ],

                  -- bgroup "Guassian Blur" [ bench "Repa V1: Uso de 2 Kernels 1x5 y 5x1"  $ nfIO gaussRepaV1,
                  --                          bench "Repa v2: Uso de 1 kernel 5x5" $ nfIO gaussRepaV2
                  --                        ],

                  -- bgroup "Mean" [ bench "Repa : Sobel"  $ nfIO sobelRepa,
                  --                  -- TODO: modificar variable accelerate cuando se realice
                  --                  bench "Accelerate: Sobel"  $ nfIO sobelRepa
                                   
                  --                ]

                  -- bgroup "Sobel" [ bench "Repa : Sobel"  $ nfIO sobelRepa,
                  --                  -- TODO: modificar variable accelerate cuando se realice
                  --                  bench "Accelerate: Sobel"  $ nfIO sobelRepa
                                   
                  --                ]
                
                ]




    putStrLn "Fin de los test"

