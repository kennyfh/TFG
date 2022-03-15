module Juicy (readImageJuicy,getAllFrames,
saveVideo
)where


{-- MODULOS IMPORTADOS-}
import Codec.Picture
import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Codec.FFmpeg.Types
import Control.Applicative
import Data.Maybe
import Control.Exception ( try, SomeException )
import Data.Tuple ( swap )
import Control.Monad ( forM_ )
import Foreign.C (CInt)

{-- TIPOS DE DATOS UTILIZADOS--}


{-- FUNCIONES--}


{-
  _                                    
 (_)  _ __ ___     __ _    __ _    ___ 
 | | | '_ ` _ \   / _` |  / _` |  / _ \
 | | | | | | | | | (_| | | (_| | |  __/
 |_| |_| |_| |_|  \__,_|  \__, |  \___|
                          |___/        

-}

-- Función genérica que nos lee las imágenes en RGB8 en formato
-- JuicyPixels
readImageJuicy :: FilePath -> IO (Image PixelRGB8)
readImageJuicy path = do
    img <- readImage path
    case img of
        Left e -> fail e
        Right img -> return $ convertRGB8 img -- En este proyecto se trabaja con RGB8


{-
  _____                                         
 |  ___|  _ __    __ _   _ __ ___     ___   ___ 
 | |_    | '__|  / _` | | '_ ` _ \   / _ \ / __|
 |  _|   | |    | (_| | | | | | | | |  __/ \__ \
 |_|     |_|     \__,_| |_| |_| |_|  \___| |___/
                                                
-}

{--------------------------------------------------------------------------------------------
getAllFrames
---------------------------------------------------------------------------------------------}
getAllFrames :: FilePath  -> IO [(Double, Image PixelRGB8)]
getAllFrames vidPath = do
    initFFmpeg
    result <- try (imageReaderTime $ File vidPath) :: IO (Either SomeException (IO (Maybe (Image PixelRGB8, Double)), IO()))
    case result of
        Left ex -> do
            putStrLn "Invalid video-path or invalid video-format detected."
            return []
        Right (getFrame, _) -> addNextFrame getFrame []

{--------------------------------------------------------------------------------------------
addNextFrame
---------------------------------------------------------------------------------------------}
addNextFrame :: IO (Maybe (Image PixelRGB8, Double)) -> [(Double, Image PixelRGB8)] -> IO [(Double, Image PixelRGB8)]
addNextFrame getFrame frames = do
    frame <- getFrame
    case frame of
        Nothing -> do
            putStrLn "No more frames found."
            return frames
        Just fr      -> do
            let newFrameData = swap fr --  swap <$> getFrame
            putStrLn ("Frame: " ++ show (length frames) ++ " added.")
            addNextFrame getFrame (frames ++ [newFrameData])

{--------------------------------------------------------------------------------------------
saveVideo 
---------------------------------------------------------------------------------------------}
saveVideo :: [(Double,Image PixelRGB8)] -> FilePath -> IO ()
saveVideo x path = do
    initFFmpeg
    frame <- imageWriter params path
    forM_ img_ls (frame . Just)
    frame Nothing
    where img_ls = map snd x
          seconds_ls = map fst x
          params :: EncodingParams
          params = defaultParams' w h fps
          w =  fromIntegral $ imageWidth $ head img_ls
          h = fromIntegral $ imageHeight $ head img_ls
          fps = getFPS seconds_ls

{--------------------------------------------------------------------------------------------
defaultParams'
---------------------------------------------------------------------------------------------}
defaultParams' :: CInt -> CInt -> Int-> EncodingParams
defaultParams' w h fps= EncodingParams w h fps Nothing Nothing "" Nothing

{--------------------------------------------------------------------------------------------
getFPS
---------------------------------------------------------------------------------------------}
getFPS :: [Double] -> Int
getFPS ls = div nframes (ceiling lasttime)
        where
              lasttime = last ls
              nframes = length ls



{-- TEST --}
test :: IO ()
test  = putStrLn "Hello World"