-- |
-- Module:      : Backend
-- Copyright    : [2022] Kenny Jesús Flores Huamán
-- License      : BSD3
--
-- Maintainer   : Kenny Jesús Flores Huamán <kennyjesus@pm.me>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)

-- Modificación del backend creado en accelerate-examples por Trevor L. McDonell, para tener que evitar tener que usar el pragma CPP,
-- además de eliminar funciones muy expecíficas de esa librería :
-- https://github.com/AccelerateHS/accelerate-examples/blob/a973ee423b5eadda6ef2e2504d2383f625e49821/lib/Data/Array/Accelerate/Examples/Internal/Backend.hs

-- Backend para cambiar entre las distintas formas de ejecutar código acelerado
module Backend
  where

import Prelude                                                      as P
import Data.Array.Accelerate
import Data.Array.Accelerate.Trafo                                  ( Afunction )
import Data.Array.Accelerate.Trafo.Sharing                          ( AfunctionR )
import qualified Data.Array.Accelerate.Interpreter                  as Interp
import qualified Data.Array.Accelerate.LLVM.Native                  as CPU
import qualified Data.Array.Accelerate.LLVM.PTX                     as PTX


-- | Ejecutar expresiones de accelerates
--
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
run CPU         = CPU.run
run PTX         = PTX.run

run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 = runN

runN :: Afunction f => Backend -> f -> AfunctionR f
runN Interpreter = Interp.runN
runN CPU         = CPU.runN
runN PTX         = PTX.runN


-- Conjunto de backends que podemos utilizar
data Backend = Interpreter -- ^ Ejecuta código accelerate usando el intérprete de Accelerate (lento)
             | CPU -- ^ Ejecutamos el código accelerate paralelizando mediante CPU
             | PTX -- ^ Ejecutamos el código accelerate paralelizando mediante NVIDIA GPU
  deriving (P.Eq, P.Enum, P.Bounded)

-- Función que selecciona el backend que queramos utilizar
selectBackend :: Int -> Backend
selectBackend 0 = Interpreter
selectBackend 1 = CPU
selectBackend 2 = PTX
selectBackend _  = error "No existe ningún backend con ese número de entrada"


-- Usa el interprete por defecto
defaultBackend :: Backend
defaultBackend =
  case maxBound of
    Interpreter -> Interpreter
    _           -> succ Interpreter


