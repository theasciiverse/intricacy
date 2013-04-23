\begin{document}

An abstract hex board type.

We coordinatize by the integral points of the hyperplane x+y+z=0:

Some hopefully elucidatory diagrams:

   .   .
   v.        u = (1,0,-1)
 .   .___.   v = (-1,1,0)
   w,  u     w = (0,-1,1)
   .   .
                                   X
                             -2-1 0
       Y                     , , , 1
     . | .               2 -. . * , 2
       |                1 -. . * * ,     * : "principal hextant"
   .   .   .         Y 0 -. . 0 . .           x>=0&&y>0
     /   \             -1 -. . . . `
   / .   . \            -2 -. . . `-2
  Z          X               ` ` `-1
                              2 1 0
                                 Z


\begin{code}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Hex where

import Data.Ix
import Data.Monoid
import Data.Ratio
   
data HexVec = HexVec {hx,hy,hz :: Int} deriving (Eq, Ord, Show)


hu = HexVec    1    0 (-1)
hv = HexVec (-1)    1    0
hw = HexVec    0 (-1)    1

hv2tup :: HexVec -> (Int,Int,Int)
hv2tup (HexVec x y z) = (x,y,z)

tup2hv :: (Int,Int,Int) -> HexVec
tup2hv (x,y,z) 
    | x+y+z == 0 = HexVec x y z

hv2tupxy :: HexVec -> (Int,Int)
hv2tupxy (HexVec x y z) = (x,y)

tupxy2hv :: (Int,Int) -> HexVec
tupxy2hv (x,y) = HexVec x y (-(x+y))

hexLen :: HexVec -> Int
hexLen (HexVec x y z) = maximum $ map abs [x,y,z]

hexDot :: HexVec -> HexVec -> Int
hexDot (HexVec x y z) (HexVec x' y' z') = x*x'+y*y'+z*z'

hextant :: HexVec -> Int
-- ^undefined at zero
--	` 1 '
--	2` '0
--	--*--
--	3' `5
--	' 4 `
hextant (HexVec x y z)
    | x >= 0 && y > 0 = 0
    | -z >= 0 && -x > 0 = 1
    | y >= 0 && z > 0 = 2
    | -x >= 0 && -y > 0 = 3
    | z >= 0 && x > 0 = 4
    | -y >= 0 && -z > 0 = 5
    | otherwise = undefined

rotate :: Int -> HexVec -> HexVec
rotate 0 v = v
rotate 2 (HexVec x y z) = HexVec z x y
rotate (-2) (HexVec x y z) = HexVec y z x
rotate 1 v = neg $ rotate (-2) v
rotate (-1) v = neg $ rotate 2 v
rotate n v | n < 0 = rotate (n+6) v
	| n > 6 = rotate (n-6) v
	| otherwise = rotate (n-2) (rotate 2 v)

cmpAngles :: HexVec -> HexVec -> Ordering
-- ^ordered by angle, taking cut along u
cmpAngles v@(HexVec x y z) v'@(HexVec x' y' z')
    | v == zero && v' == zero = EQ
    | v == zero = LT
    | compare (hextant v) (hextant v') /= EQ = 
	compare (hextant v) (hextant v')
    | hextant v /= 0 =
	cmpAngles (rotate (-(hextant v)) v) (rotate (-(hextant v)) v')
    | otherwise = compare (x'%y') (x%y)

instance Ix HexVec where
    range (h,h') = 
	[ tupxy2hv (x,y) | (x,y) <- range (hv2tupxy h, hv2tupxy h') ]
    inRange (h,h') h'' =
	inRange (hv2tupxy h, hv2tupxy h') (hv2tupxy h'')
    index (h,h') h'' =
	index (hv2tupxy h , hv2tupxy h') (hv2tupxy h'')

-- HexDirs are intended to be HexVecs of length <= 1
type HexDir = HexVec
isHexDir v = hexLen v == 1

hexDirs = map (\n -> rotate n hu) [0..5]

--minusHu = HexVec    (-1) 1    0
--minusHv = HexVec    0    (-1) 1
--minusHw = HexVec    1    0    (-1)

canonDir :: HexDir -> HexDir
canonDir dir | dir `elem` [ hu, hv, hw ] = dir
	     | isHexDir dir = canonDir $ neg dir
	     | dir == zero = zero
	     | otherwise = undefined
\end{code}
     
Some general stuff on groups and actions and principal homogeneous spaces. We
use additive notation, even though there's no assumption of commutativity.

\begin{code}
class Monoid g => Grp g where
    neg :: g -> g
    zero :: g
    zero = mempty

infixl 6 <+>
infixl 6 <->
class Action a b where
    (<+>) :: a -> b -> b
instance Monoid m => Action m m where
    (<+>) = mappend

class Differable a b c where
    (<->) :: a -> b -> c
instance Grp g => Differable g g g where
    x <-> y = x <+> (neg y)

newtype PHS g = PHS { getPHS :: g }
    deriving (Eq, Ord, Show)

instance Grp g => Action g (PHS g) where
    x <+> (PHS y) = PHS (x <+> y)
instance Grp g => Differable (PHS g) (PHS g) g where
    (PHS x) <-> (PHS y) = x <-> y
    
infixl 7 <*>
class MultAction a b where
    (<*>) :: a -> b -> b
    
instance (Grp a, Integral n) => MultAction n a where
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
