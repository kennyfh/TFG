module JuicyRepa where


{-- MODULOS IMPORTADOS-}
import Juicy
import Codec.Picture
import Data.Array.Repa
import Data.Array.Repa.Repr.Unboxed as U
import Codec.Picture.Types
import Control.Monad (forM)

{-- TIPOS DE DATOS UTILIZADOS--}
type Channel a = Array U DIM2 a
type ImgRGB a = [Channel a]

{-- FUNCIONES--}

jcyToRepa :: Monad m => Image PixelRGB8 -> m (Channel (Pixel8,Pixel8,Pixel8))
jcyToRepa img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData} =
    computeP $ fromFunction (Z :. imageWidth :. imageHeight) (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))



readImageIntoRepa :: FilePath -> IO (ImgRGB Pixel8)
readImageIntoRepa path = do
    a1 <- readImageJuicy path -- Image PixelRGB8
    a2 <- jcyToRepa a1 -- Channel (Pixel8,Pixel8,Pixel8)
    let (r,g,b) = U.unzip3 a2 -- unzip -> Dada un array de 3-tupla, los divide en 3 arrays distintos O(1)
    return [r,g,b]
   

repaToJuicy :: ImgRGB Pixel8 -> Image PixelRGB8 
repaToJuicy img = generateImage func w h
    where [r,g,b] = img
          Z :. w :. h = extent r
          func x y = 
              let r' =  r ! (Z :. x :. y)
                  g' =  g ! (Z :. x :. y)
                  b' =  b ! (Z :. x :. y)
               in PixelRGB8 r' g' b'


exportBW :: Channel Float -> Image PixelF  
exportBW img = generateImage func w h
    where Z :. w :. h = extent img
          func x y = img ! (Z :. x :. y)


exportBand :: Channel Pixel8 -> Image Pixel8
exportBand img = generateImage func w h
    where Z :. w :. h = extent img
          func x y = img ! (Z :. x :. y)



{-- TEST --}
-- test :: IO ()
-- test  = do
--     -- test <- readImageJuicy "saitama.png"
--     test <- readImageIntoRepa "saitama.png"
--     print $ head test
--     putStrLn "Hello World"