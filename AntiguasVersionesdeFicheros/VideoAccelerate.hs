module VideoAccelerate where
-- Importación de módulos
import Juicy as J
import JuicyAccelerate as JA
import Codec.Picture as P
import Control.Parallel.Strategies
import Accelerate as A
import Data.Array.Accelerate
import Prelude as P
import Data.Array.Accelerate.LLVM.PTX as PTX
import qualified Data.Array.Accelerate as A


vidSobel :: IO()
vidSobel = do
    putStrLn "Inicio prueba video Accelarate"
    -- vpath <- getLine
    let vpath =  "data/videos/sample-5s.mp4"
    xs <-  getAllFrames vpath -- videos/video1.mp4
    let acc_xn = accelerateFrames xs
    let frames = P.map (oneCToJcy . PTX.run . A.demoteFloat . A.sobel . A.grayScale . use) acc_xn

    -- Xº Generamos el resumen del vídeo
    saveVideo frames "video.mp4"
    putStrLn "End of VideoSummarization.hs"

-- Función que nos transforma todos los frames en JuixyPixels en Accelerate
accelerateFrames :: [Image PixelRGB8] -> [Matrix RGB]
accelerateFrames xs =
    let bs = P.map JA.imgToArr xs
        cs = bs `using` parList rdeepseq
        in cs

