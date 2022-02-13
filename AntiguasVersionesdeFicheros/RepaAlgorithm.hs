module RepaAlgorithm where
import Codec.Picture as P
import Data.Array.Repa as R
import Data.Array.Repa.Unsafe as RU

prueba2 :: IO()
prueba2  = do
    let ejemplo = fromListUnboxed (Z :. 2 :. 2) [(1,1,1),(2,2,2),(3,3,3),(4,4,4)]::Array U DIM2 (Int,Int,Int)
    -- R.foldP (+) 0 ejemplo
    x <- fileToRepa "saitama.png"
    putStrLn "Histogram"
    -- 1º VERSION HISTOGRAMAS
    histograms <- computeHistograms $ doHistogramRGB x
    print histograms
    putStrLn "Fin de función"

type RGB = (Pixel8,Pixel8,Pixel8) 
type Img a = Array U DIM2 a
type Histogram = Array D DIM1 Int
type Histograms = (Histogram,Histogram,Histogram)

--lectura de Imágenes
fileToRepa :: FilePath -> IO (Img RGB)
fileToRepa path = jcyToRepa <$> rImg path

--TODO: realizarlo en paralelo
jcyToRepa :: Image PixelRGB8 -> Img RGB
jcyToRepa img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData} =
    R.computeS $ R.fromFunction (Z :. imageWidth :. imageHeight) (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))


rImg :: FilePath -> IO (Image PixelRGB8)
rImg path = do
    result <- P.readImage path
    case result of
        Left s -> fail s
        Right img -> return (convertRGB8 img) -- convertRGB8 :: DynamicImage -> Image PixelRGB8

-- blanco y negro
{- FORMULA PARA SACAR LA LUMINOSIDAD
    Y' = 0.2989 R + 0.5870 G + 0.1140 B 
    (https://en.wikipedia.org/wiki/Grayscale)
-}
toGrayScale :: Img RGB -> IO (Img Float)
toGrayScale img = R.computeP (R.map getLuminanceRGB img)::IO (Array U DIM2 Float)

getLuminanceRGB :: RGB -> Float
getLuminanceRGB (r,g,b) =
    let r' = fromIntegral (fromIntegral r ::Int) / 255
        g' = fromIntegral (fromIntegral g :: Int) / 255
        b' = fromIntegral (fromIntegral b :: Int) / 255
        in (0.2989*r' + 0.5870*g' + 0.1140*b')

{-Histogram-}
-- Seleccionamos el 3 elemento de nuestra tupla
fst' :: (a,b,c) -> a
fst' (a,_,_)=a

snd' :: (a,b,c) -> b
snd' (_,b,_)=b

third' :: (a,b,c) -> c
third' (_,_,z) = z

computeHistograms :: Histograms -> IO(Array U DIM1 Int,Array U DIM1 Int,Array U DIM1 Int)
computeHistograms (a,b,c)= do
    r <- R.computeP a
    g <- R.computeP b
    b <- R.computeP c
    return (r,g,b)
    
doHistogramRGB :: Img RGB -> Histograms
doHistogramRGB img = 
    let (Z :. nrows :. ncolums) = R.extent img
        zeros = R.fromFunction (Z :. 256) (\_ -> 0::Int)
        incElem idx x = RU.unsafeTraverse x id (\l i -> l i + if i==(Z:.fromIntegral idx) then 1 else 0)
    in Prelude.foldl (\(hstR,hstG,hstB) (rows,colums) -> 
        let val = unsafeIndex img (Z :. rows :. colums)
            r = fst' val
            g = snd' val
            b = third' val
        in (incElem r hstR, incElem g hstG,incElem b hstB))
        (zeros,zeros,zeros)
        [(rows,colums) | rows <- [0..nrows], colums <- [0..ncolums-1]]

