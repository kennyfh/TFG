module JuicyAccelerate where

{-- MODULOS IMPORTADOS-}
import Data.Array.Accelerate.IO.Codec.Picture
import Data.Array.Accelerate.IO.Codec.Picture.Types
import Juicy
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Prelude hiding (map)
import Codec.Picture

{-- TIPOS DE DATOS UTILIZADOS--}
type RGB = (Pixel8,Pixel8,Pixel8)

{-- FUNCIONES--}

readImageAcc :: FilePath -> IO (Matrix RGB)
readImageAcc path =  imgToArr <$> readImageJuicy path

imgToArr :: Image PixelRGB8 -> Matrix RGB -- (Exp Word8, Exp Word8, Exp Word8)
imgToArr img@Image{imageWidth=imageWidth, imageHeight=imageHeight, imageData=imageData} =
    fromFunction (Z :. imageHeight :. imageWidth) (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img y x
       in (r, g, b))


-- TODO: cambiar el run por Accelerate Native o Accelerate PTX
rgbToJcy :: Matrix RGB -> Image PixelRGB8 
rgbToJcy arr = imageOfArray $ prom arr
    where prom :: Matrix RGB -> Matrix PixelRGB8 
          prom mat =  run $ A.map (\p -> let (r,g,b) = unlift p in PixelRGB8_ r g b) (use arr)

-- pp :: Matrix RGB -> Matrix PixelRGB8 
-- pp arr =  run $ A.map (\p -> let (r,g,b) =  unlift p in PixelRGB8_ r g b) (use arr)

greyToJcy :: Matrix Float -> Image PixelF
greyToJcy = imageOfArray


oneCToJcy :: Matrix Pixel8 -> Image Pixel8
oneCToJcy = imageOfArray



-- grayToChnl :: Matrix Float -> Image Pixel8
-- grayToChnl = 

{-- TEST --}
test :: IO ()
test  = do
    img <- readImageAcc "images/saitama.png"
    -- let xs = pp img
    -- let xs2 = imageOfArray xs
    -- savePngImage "locora.png" (ImageRGB8 xs2)
    -- print img    
    putStrLn "Hello World"



