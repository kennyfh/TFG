module PruebasRandom where
import Data.Array.Repa
import Data.List as L 

test :: IO()
test  = do

    let arr =  fromListUnboxed (ix2 3 3) [0..8] :: Array U DIM2 Double     
    let fila =  toList $ slice arr  (Any :. (0::Int) :. All) 
    print arr
    print fila
    putStrLn "ASD"

--https://stackoverflow.com/questions/30234410/repas-slices-and-shapes/30240085#30240085

-- https://stackoverflow.com/questions/46181646/where-to-find-chunks-in-haskell
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = L.take n xs : chunk n (L.drop n xs)
