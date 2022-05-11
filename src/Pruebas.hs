laplace
    :: Monad m
    => Int -- ^ Numero de iteraciones
    -> Channel Float -- ^ Máscara
    -> Channel Float -- ^ Máscara de valores
    -> Channel Float -- ^ Imagen a utilizar
    -> m (Channel Float) -- ^ Imagen de salida 
laplace steps !arrBoundMask !arrBoundValue = go steps -- Es equivalente a: laplace steps arrBoundMask arrBoundValue imgInit = go steps imgInit
    where go 0 !img =  return img
          go n !img = do img' <- relaxLaplace img
                         go (n-1) img'

          relaxLaplace mat = R.computeP
                             $ R.szipWith (+) arrBoundMask
                             $ R.szipWith (*) arrBoundMask
                             $ R.smap (/4)
                             $ mapStencil2 (BoundConst 0)
                                [stencil2|   0 1 0
                                             1 0 1 
                                             0 1 0 |] mat
{-# NOINLINE laplace #-}


-- | Extract the boundary value from a RGB triple.
arrBoundValueFunc :: (Pixel8, Pixel8, Pixel8) -> Float
{-# INLINE arrBoundValueFunc #-}
arrBoundValueFunc (!r, !g, !b)
        -- A non-boundary value.
        | r == 0 && g == 0 && b == 255
        = 0

        -- A boundary value.
        | (r == g) && (r == b)
        = fromIntegral (fromIntegral r :: Int) / 255

        | otherwise
        = error "Unhandled pixel value in input "


-- |  boundary mask from a RGB triple.
arrBoundMaskFunc :: (Pixel8, Pixel8, Pixel8) -> Float
{-# INLINE arrBoundMaskFunc #-}
arrBoundMaskFunc (!r, !g, !b)
        -- A non-boundary value.
        | r == 0 && g == 0 && b == 255
        = 1

        -- A boundary value.
        | (r == g) && (r == b)
        = 0

        | otherwise= error "Unhandled pixel value in input "

