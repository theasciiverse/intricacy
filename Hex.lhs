\begin{document}

An abstract hex board type.

We co-ordinatize by the integer points of the hyperplane x+y+z=0:

\begin{code}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Hex where

import Ix
import Data.Monoid
   
data HexVec = HexVec {hx,hy,hz :: Int} deriving (Eq, Ord, Show)

hu = HexVec    1 (-1)    0
hv = HexVec    0    1 (-1)
hw = HexVec (-1)    0    1

hv2tup :: HexVec -> (Int,Int,Int)
hv2tup (HexVec x y z) = (x,y,z)

tup2hv :: (Int,Int,Int) -> HexVec
tup2hv (x,y,z) 
    | x+y+z == 0 = HexVec x y z

hv2tupxy :: HexVec -> (Int,Int)
hv2tupxy (HexVec x y z) = (x,y)

tupxy2hv :: (Int,Int) -> HexVec
tupxy2hv (x,y) = HexVec x y (-(x+y))

instance Ix HexVec where
    range (h,h') = 
	[ tupxy2hv (x,y) | (x,y) <- range (hv2tupxy h, hv2tupxy h') ]
    inRange (h,h') h'' =
	inRange (hv2tupxy h, hv2tupxy h') (hv2tupxy h'')
    index (h,h') h'' =
	index (hv2tupxy h , hv2tupxy h') (hv2tupxy h'')

-- HexDirs are intended to be HexVecs of length <= 1
type HexDir = HexVec
\end{code}
     
Some general stuff on groups and actions and principal homogeneous spaces. We
use additive notation, even though there's no assumption of commutativity.

\begin{code}
class Monoid g => Grp g where
    neg :: g -> g
    zero :: g
    zero = mempty

class Action a b where
    (<+>) :: a -> b -> b
instance Monoid m => Action m m where
    (<+>) = mappend

class Differable a b c where
    (<->) :: a -> b -> c
instance Grp g => Differable g g g where
    x <-> y = x <+> (neg y)

newtype Grp g => PHS g = PHS { getPHS :: g }
    deriving (Eq, Ord, Show)

instance Grp g => Action g (PHS g) where
    x <+> (PHS y) = PHS (x <+> y)
instance Grp g => Differable (PHS g) (PHS g) g where
    (PHS x) <-> (PHS y) = x <-> y
    
class MultAction a b where
    (<*>) :: a -> b -> b
    
instance Grp a => MultAction Int a where
    0 <*> _              = zero
    1 <*> x              = x
    n <*> x
	| n < 0     = (-n) <*> (neg x)
	| even n    = (n `div` 2) <*> (x <+> x)
	| otherwise = x <+> ((n `div` 2) <*> (x <+> x))

\end{code}

Now we define HexSpaces as spaces acted on by HexVec, and with a canonical
HexVec difference between two points (e.g. PHS HexVec).

\begin{code}

instance Monoid HexVec where
    (HexVec x y z) `mappend` (HexVec x' y' z') = HexVec (x+x') (y+y') (z+z')
    mempty = HexVec 0 0 0
instance Grp HexVec where
    neg (HexVec x y z) = HexVec (-x) (-y) (-z)

class (Action HexVec b, Differable b b HexVec) => HexSpace b
instance HexSpace (PHS HexVec)

\end{code}

Testing:

\begin{code}
r = range (tup2hv (-3,-3,6), tup2hv (3,3,-6))
test1 = index (tup2hv (-3,-3,6), tup2hv (3,3,-6)) (r!!5) == 5

a :: PHS HexVec
a = PHS zero
test2 = hu <+> a
\end{code}

\end{document}
