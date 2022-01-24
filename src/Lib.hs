module Lib
(getAllFrames,
saveVideo

) where

-- Módulos utilizados
-- Módulos utilizados
import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Codec.FFmpeg.Types
import Codec.Picture
import Control.Applicative
import Data.Maybe
import Control.Exception
import Data.Tuple
import Control.Monad
import Foreign.C (CInt)


getAllFrames :: FilePath  -> IO [(Double, Image PixelRGB8)]
getAllFrames vidPath = do
    initFFmpeg
    result <- try (imageReaderTime $ File vidPath) :: IO (Either SomeException (IO (Maybe (Image PixelRGB8, Double)), IO()))
    case result of
        Left ex -> do
            putStrLn "Invalid video-path or invalid video-format detected."
            return []
        Right (getFrame, _) -> addNextFrame getFrame []

addNextFrame :: IO (Maybe (Image PixelRGB8, Double)) -> [(Double, Image PixelRGB8)] -> IO [(Double, Image PixelRGB8)]
addNextFrame getFrame frames = do
    frame <- getFrame
    case frame of
        Nothing -> do
            putStrLn "No more frames found."
            return frames
        Just fr       -> do
            let newFrameData = swap fr
            putStrLn ("Frame: " ++ show (length frames) ++ " added.")
            addNextFrame getFrame (frames ++ [newFrameData])


{--------------------------------------------------------------------------------------------
saveVideo 
---------------------------------------------------------------------------------------------}
saveVideo :: [(Double,Image PixelRGB8)] -> FilePath -> IO ()
saveVideo x path = do
    initFFmpeg
    frame <- imageWriter params path
    -- putStrLn $ show fps
    forM_ img_ls (frame . Just)
    frame Nothing
    where img_ls = map snd x
          seconds_ls = map fst x
          params :: EncodingParams
          params = defaultParams' w h fps
          w =  fromIntegral $ imageWidth $ head img_ls
          h = fromIntegral $ imageHeight $ head img_ls
          fps = getFPS seconds_ls

defaultParams' :: CInt -> CInt -> Int-> EncodingParams 
defaultParams' w h fps= EncodingParams w h fps Nothing Nothing "" Nothing


getFPS :: [Double] -> Int
getFPS ls = div nframes (ceiling lasttime)
        where 
              lasttime = last ls
              nframes = length ls