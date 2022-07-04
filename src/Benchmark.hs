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
import Data.Array.Repa.Repr.Unboxed as U

-- Accelerate
import qualified Data.Array.Accelerate as A
import Backend as B


-- Algoritmos Realizados
import qualified Repa as R
import qualified Accelerate as A

-- Lectura Juicy
import JuicyAccelerate
import JuicyRepa
import Accelerate
import Codec.Picture
import Control.Parallel.Strategies
import Juicy (saveVideo, getAllFrames)
import qualified Data.Array.Accelerate.LLVM.PTX as PTX
import Data.Array.Accelerate.IO.Codec.Picture
import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Interpreter as Native

---- 



-- Función de prueba para testear Criterion: https://github.com/haskell/criterion/blob/master/examples/Fibber.hs
fib m | m P.< 0     = error "negative!"
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

-- MAIN
testB :: IO()
testB = do
    putStrLn "Bienvenido:"
    mapM_ putStrLn ["1 : Realiza benchmarks algoritmos", "2 : Muestra dataset (Reviews)","3 : Salir del menú"]
    putStrLn "Seleccione una opción:"
    x<-getLine
    opciones x

-- (opciones op) Según el String (op) nos ejecuta las funciones que correspondan
opciones :: String -> IO()
opciones "1" = do
    putStrLn "Inicializando benchmarks"
    testBenchmarks
opciones "2" = do
    putStrLn "Las imágenes resultantes se guardarán en el directorio /output"
    testImages
opciones "3" = do
    putStrLn "Saliendo del menú...."
opciones _  = do
    putStrLn "Lo siento, la opción no es correcta, vuelve a intentarlo"
    testB



testBenchmarks :: IO ()
testBenchmarks = do
    putStrLn "Inicio de los test"
    -- Imagen de prueba
    imgRepa <- readImageIntoRepa "data/images/lena_color.png"
    imgAcc <- readImageAcc "data/images/lena_color.png"

    defaultMain [
                  bgroup "Histogram" [ bench "Repa V1: Sequencial"  $ nfIO (R.generateHistogramsV1 <$> mapM R.promoteInt imgRepa),
                                       bench "Repa V2: Cada canal en paralelo"  $ nfIO (R.generateHistogramsV2 <$> mapM R.promoteInt imgRepa),
                                       bench "Repa V3: Cada canal y fila en paralelo"  $ nfIO (R.generateHistogramsV3 <$> mapM R.promoteInt imgRepa),
                                       bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) gHistogram) imgAcc,
                                       bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) gHistogram) imgAcc,
                                       bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                       bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) gHistogram) imgAcc
                                       ],

                  bgroup "GreyScale" [ bench "Repa V1: Uso de zipWith"  $ whnfIO (R.toGrayScaleV1 imgRepa),
                                       bench "Repa v2: Uso de zip3" $ whnfIO (R.toGrayScaleV2 imgRepa),
                                       bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) A.grayScale) imgAcc,
                                       bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) A.grayScale) imgAcc,
                                       bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                       bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) A.grayScale) imgAcc
                                       ]

                  ,bgroup "Gaussian Blur (RGB) Uso kernel 1x5 y 5x1" [ bench "Repa V1: Uso de 2 Kernels 1x5 y 5x1"  $ whnfIO (mapM (R.promote >=> R.blurV1 1) imgRepa),
                                           bench "AccelerateV1 : Interprete" $ whnf (B.run1 (selectBackend 0) A.blurRGB) imgAcc,
                                           bench "AccelerateV1 : CPU" $ whnf (B.run1 (selectBackend 1) A.blurRGB) imgAcc,
                                           bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                           bench "AccelerateV1 : GPU" $ whnf (B.run1 (selectBackend 2) A.blurRGB) imgAcc

                  ],

                  bgroup "Gaussian Blur (RGB) Uso kernel 5x5 " [ bench "Repa v2: Uso de 1 kernel 5x5" $ whnfIO (mapM (R.promote >=> R.blurV2 1) imgRepa),
                                           bench "AccelerateV2 : Interprete" $ whnf (B.run1 (selectBackend 0) (A.gaussianSmoothingRGB . promoteImageF)) imgAcc,
                                           bench "AccelerateV2 : CPU" $ whnf (B.run1 (selectBackend 1) (A.gaussianSmoothingRGB . promoteImageF)) imgAcc,
                                           bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                           bench "AccelerateV2 : GPU" $ whnf (B.run1 (selectBackend 2) (A.gaussianSmoothingRGB . promoteImageF)) imgAcc

                  ],

                  bgroup "Mean" [ bench "Repa : Mean Filter"  $ whnfIO (mapM (R.promote >=> R.meanF) imgRepa),
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
                  bgroup "Laplace" [ bench "Repa: Paralelismo CPU"  $ whnfIO (R.laplace <$> R.toGrayScaleV1 imgRepa),
                                   bench "Accelerate : Interprete" $ whnf (B.run1 (selectBackend 0) (A.laplace . A.grayScale)) imgAcc,
                                   bench "Accelerate : CPU" $ whnf (B.run1 (selectBackend 1) (A.laplace . A.grayScale)) imgAcc,
                                   bench "Descanso entre GPU y CPU"  $ whnf fib 1,
                                   bench "Accelerate : GPU" $ whnf (B.run1 (selectBackend 2) (A.laplace . A.grayScale)) imgAcc
                                   ]
                ]

    putStrLn "Fin de los test"



{-Test de imágenes -}

testImages :: IO ()
testImages =  do
  putStrLn "Inicializando el test"
  -- Histograma
  testHistograma
  -- Grayscale
  testGrayScale
  -- Mean 
  testMean
  -- Gauss 
  testGaussBlur
  -- Sobel
  testSobel
  --laplace
  testLaplace
  putStrLn "Fin del test"




testGrayScale :: IO()
testGrayScale = do

  -- Prueba Imagen 1 : Accelerate
  imgAcc <- readImageAcc "data/images/PhotoMachupichu.png"
  let grAcc =  B.run1 (selectBackend 2) A.grayScale imgAcc
  savePngImage "machupichuGrayScale.png" (ImageYF $ greyToJcy grAcc)

  -- Prueba Imagen 2 : Repa
  imgRepa <- readImageIntoRepa "data/images/flyAda.jpg"
  grRepa <- R.toGrayScaleV2 imgRepa
  savePngImage "flyAdaGrayScale.png" (ImageYF $ exportBW grRepa)


  putStrLn "Fin prueba"



testSobel :: IO ()
testSobel =  do

  -- https://unsplash.com/photos/iWMfiInivp4
  -- Prueba Imagen 1 : Accelerate
  imgAcc <- readImageAcc "data/images/tajmahal.jpg"
  let sobelAcc = B.run1 (selectBackend 2) (A.sobel . A.grayScale) imgAcc
  savePngImage "sobelAcc.png" (ImageYF $ greyToJcy sobelAcc)

  -- Prueba Imagen 2 : Repa
  imgRepa <- readImageIntoRepa "data/images/lena_color.png"
  imgGreyRepa <- R.toGrayScaleV2 imgRepa
  outputR <- R.sobel imgGreyRepa
  savePngImage "sobelrepa.png" (ImageYF $ exportBW outputR)


  putStrLn "Fin test Sobel"

testHistograma :: IO ()
testHistograma =  do
  --Accelerate
  -- https://pixabay.com/es/photos/aurora-borealis-noruega-noche-playa-1032517/
  imgAcc <- readImageAcc "data/images/aurolaboreal.jpg"
  let (r,g,b) = A.unzip3 $ gHistogram (A.use imgAcc)
  print $ B.run (selectBackend 2) r
  print $ B.run (selectBackend 2) g
  print $ B.run (selectBackend 2) b
  putStrLn "============================="
  --Repa
  -- https://pixabay.com/es/photos/alpaca-lana-pelo-herb%c3%advoros-cabeza-4357188/
  imgRepa <- readImageIntoRepa "data/images/alpaca.jpg"
  hstVector <- R.generateHistogramsV3 <$> mapM R.promoteInt imgRepa
  print hstVector
  putStrLn "Fin histograma"


testMean :: IO()
testMean =  do
  -- Para añadir poisson noise en las imágenes, hemos decidido utilizar 
  -- https://imagej.nih.gov/ij/plugins/poisson-noise.html
  -- para poder añadir esto


  -- Prueba Imagen 1 : Accelerate
  imgAcc <- readImageAcc "data/images/skyWithPoissonNoise.png"
  let meanAcc = B.run1 (selectBackend 2) (A.demoteimageP . A.meanRGBFilter . A.promoteImageF) imgAcc
  savePngImage "skyMean.png" (ImageRGB8 $ rgbToJcy meanAcc)

  -- Prueba Imagen 2 : Repa
  imgRepa <- readImageIntoRepa "data/images/trujilloPoissonNoise.png"
  grRepa <- (mapM $ R.promote >=> R.meanF >=> R.demote) imgRepa
  savePngImage "trujilloMean.png" (ImageRGB8 $ repaToJuicy grRepa)

  putStrLn "FIn del mean "


testGaussBlur :: IO ()
testGaussBlur = do

  -- Acc
  -- https://pixabay.com/es/photos/%c3%a1rbol-lago-estrellas-reflexi%c3%b3n-838667/
  imgAcc <- readImageAcc "data/images/treegaussnoise.png"
  let gausAcc =  B.run1 (selectBackend 2) (A.demoteimageP . A.blurRGBV2 . A.blurRGBV2 . A.promoteImageF) imgAcc
  savePngImage "treeGauss.png" (ImageRGB8 $ rgbToJcy gausAcc)

  -- Repa
  imgRepa <- readImageIntoRepa "data/images/catwithgaussnoise.png"
  gaussRepaV1RGB <-  mapM (R.promote >=> R.blurV1 2 >=> R.demote) imgRepa
  savePngImage "catGauss.png" (ImageRGB8 $ repaToJuicy gaussRepaV1RGB)
  putStrLn "FIn del gauss blur"


testLaplace :: IO ()
testLaplace = do
  -- https://unsplash.com/photos/vjDbHCjHlEY
  imgAcc <- readImageAcc "data/images/asialpl.jpg"
  let imgL = B.run1 (selectBackend 2) (A.laplace . A.grayScale) imgAcc
  savePngImage "asialpl.png" (ImageYF $ greyToJcy imgL)

  --https://unsplash.com/photos/HXNwatPDGic
  imgRepa <- readImageIntoRepa "data/images/wallslpl.jpg"
  lplRepa <- R.toGrayScaleV2 imgRepa
  outputR <- R.laplace lplRepa
  savePngImage "wallsLaplaceFilter.png" (ImageYF $ exportBW outputR)
  putStrLn "Fin del test"


  ----------------------------------------------------------------------------
  ---------------------------------------------------------------------------
vidGaussAcc:: IO()
vidGaussAcc = do
    putStrLn "Inicio prueba video Accelarate"
    -- vpath <- getLine
    let vpath =  "data/videos/sample5s.mp4"
    xs <-  getAllFrames vpath -- videos/video1.mp4
    let frames = P.map (B.run (selectBackend 2) . A.demoteimageP . A.blurRGBV2 .  A.blurRGBV2 . A.promoteImageF . A.use . imgToArr) xs
    putStrLn "entramos a 2"
    let frames2 = P.map rgbToJcy2 frames
    saveVideo frames2 "videoK.mp4"
    putStrLn "Final prueba video (Accelerate)"

-- TODO:
rgbToJcy2 :: A.Matrix RGB -> Image PixelRGB8
rgbToJcy2 arr = imageOfArray $ prom arr
    where prom :: A.Matrix RGB -> A.Matrix PixelRGB8
          prom mat =  Native.run $ A.map (\p -> let (r,g,b) = unlift p in PixelRGB8_ r g b) (use arr)



-- -- Función que nos transforma todos los frames en JuixyPixels en Accelerate
-- accelerateFrames :: [Image PixelRGB8] -> [A.Matrix RGB]
-- accelerateFrames xs =
--     let bs = P.map imgToArr xs
--         cs = bs `using` parList rdeepseq
--         in cs


vidGaussRepa ::  IO ()
vidGaussRepa = do
  putStrLn "Inicio del test de video (Repa)"
  let vpath =  "data/videos/sample5s.mp4"
  xs <-  getAllFrames vpath
  putStrLn "Fin de lectura de frames a Juicy"
  repa_xn <- mapM fun xs
  saveVideo repa_xn "videoKRepa.mp4"
  putStrLn "Final prueba video (Accelerate)"


fun :: Image PixelRGB8 -> IO (Image PixelRGB8)
fun img = do
    asd <- jcyToRepa img
    let (r,g,b) = U.unzip3 asd
    xs <- mapM (R.promote >=> R.blurV1 2 >=> R.demote) [r,g,b]
    return (repaToJuicy xs)

