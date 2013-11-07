module Util where
import qualified Data.Vector as Vector
import Data.Vector (Vector)

enumVec :: Vector a -> [(Int,a)]
enumVec = Vector.toList . Vector.indexed
