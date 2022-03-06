gaussianFilter :: ImgRGB -> IO ImgRGB
gaussianFilter img = do
    r' <- gGaussianChannel r
    g' <- gGaussianChannel g
    b' <- gGaussianChannel b
    return [r',g',b']
    where [r,g,b] = img

gGaussianChannel :: Channel Int -> IO (Channel Int)
gGaussianChannel ch = R.computeP $ forStencil2 BoundClamp ch gKernel
{-# NOINLINE gGaussianChannel #-}

-- Kernel Gaussiano
{-# INLINE gKernel #-}
gKernel :: Stencil DIM2 Int
gKernel = [stencil2| 1 2 1
                     2 4 2
                     1 2 1 |]


-- demote :: Monad m => Channel Float -> m (Channel Pixel8)
-- demote =  computeP . R.map func
--     where {-# INLINE func #-}
--           func :: Float -> Pixel8
--           func x = fromIntegral (truncate x :: Int)
-- {-# NOINLINE demote #-}