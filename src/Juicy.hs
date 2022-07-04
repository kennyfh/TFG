-- |
-- Module:      : Juicy
-- Copyright    : [2022] Kenny Jesús Flores Huamán
-- License      : BSD3
--
-- Maintainer   : Kenny Jesús Flores Huamán <kennyjesus@pm.me>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
{-# LANGUAGE RankNTypes #-}
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
getAllFrames :: FilePath  -> IO [Image PixelRGB8]
getAllFrames path = do
    -- Inicializamos el ffmpeg con todos los codecs registrados
    initFFmpeg
    -- Leemos los frames
    frames <- try (imageReader $ File path) :: IO (Either SomeException (IO (Maybe (Image PixelRGB8)), IO()))
    case frames of
        -- Si falla el codec o no carga el video, devolvemos un error por consola
        Left error -> do
            putStrLn "Invalid video-path or invalid video-format detected. "
            return []
        -- En caso contrario, vamos a leer los frames para guardarlos en una lista
        Right (getFrame, _) -> addNextFrame getFrame []

{--------------------------------------------------------------------------------------------
addNextFrame
---------------------------------------------------------------------------------------------}
addNextFrame :: IO (Maybe (Image PixelRGB8)) -> [Image PixelRGB8] -> IO [Image PixelRGB8]
addNextFrame getFrame frames = do
    -- Seleccionamos el frame
    frame <- getFrame
    case frame of
        -- En el caso de que no encuentre más frames, devolvemos la lista de frames
        Nothing -> do
            putStrLn "No more frames found."
            return frames
        -- En caso contrario    
        Just fr      -> do
            putStrLn ("Frame: " ++ show (length frames) ++ " added.")
            addNextFrame getFrame (frames ++ [fr])

{--------------------------------------------------------------------------------------------
saveVideo 
---------------------------------------------------------------------------------------------}
-- saveVideo :: [Image PixelRGB8] -> FilePath -> IO ()
saveVideo :: forall p. JuicyPixelFormat p => [Image p] -> FilePath -> IO()
saveVideo images path = do
    -- Inicializamos el ffmpeg con todos los codecs registrados
    initFFmpeg
    -- Abrimos un archivo de destino donde vamos a escribir el vídeo
    frame <- imageWriter params path
    -- Por cada frame, 
    forM_ images (frame . Just)
    -- Cerramos de forma correcta para poder finalizar la codificación del vídeo
    -- poniendo un Nothing al final
    frame Nothing
    where params :: EncodingParams 
          params = defaultParams' w h 24 -- función que da parámetros predeterminados para un video
          w =  fromIntegral $ imageWidth $ head images -- width
          h = fromIntegral $ imageHeight $ head images -- height

{--------------------------------------------------------------------------------------------
defaultParams'
---------------------------------------------------------------------------------------------}
defaultParams' :: CInt -> CInt -> Int-> EncodingParams
defaultParams' w h fps= EncodingParams w h fps Nothing Nothing "" Nothing

{--------------------------------------------------------------------------------------------
getFPS
---------------------------------------------------------------------------------------------}
-- getFPS :: [Double] -> Int
-- getFPS ls = div nframes (ceiling lasttime)
--         where
--               lasttime = last ls
--               nframes = length ls
