module BinaryInstances where
import Data.Binary
import qualified Data.Vector as Vector
import Control.Monad
import Data.Int (Int8)

import GameStateTypes
import Physics
import Hex
import Frame

newtype SmallInt = SmallInt {fromSmallInt :: Int}
newtype SmallNat = SmallNat {fromSmallNat :: Int}
instance Binary SmallNat where
    put (SmallNat n) = if n < 255 then putWord8 (fromIntegral n) else putWord8 255 >> put n
    get = do
	n' <- get :: Get Word8
	if n' == 255 then liftM SmallNat get else return $ SmallNat $ fromIntegral n'

instance Binary SmallInt where
    put (SmallInt n) = if abs n < 127 then put ((fromIntegral n)::Int8) else put (127::Int8) >> put n
    get = do
	n' <- get :: Get Int8
	if n' == 127 then liftM SmallInt get else return $ SmallInt $ fromIntegral n'

putPackedNat,putPackedInt :: Int -> Put
putPackedNat n = put $ SmallNat n
putPackedInt n = put $ SmallInt n
getPackedNat,getPackedInt :: Get Int
getPackedNat = liftM fromSmallNat get
getPackedInt = liftM fromSmallInt get

newtype ShortList a = ShortList {fromShortList :: [a]}
instance Binary a => Binary (ShortList a) where
    put (ShortList as) = putPackedNat (length as) >> mapM_ put as
    get = do
	n <- getPackedNat
	ShortList `liftM` getMany n

 -- | 'getMany n' get 'n' elements in order, without blowing the stack.
 -- [ copied from source of package 'binary' by Lennart Kolmodin ]
getMany :: Binary a => Int -> Get [a]
getMany = go []
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

instance Binary HexVec where
    put (HexVec x y _) = putPackedInt x >> putPackedInt y
    get = do 
	x <- getPackedInt
	y <- getPackedInt
	return $ tupxy2hv (x,y)
instance Binary g => Binary (PHS g) where
    put (PHS v) = put v
    get = liftM PHS get
instance Binary GameState where
    put (GameState pps conns) = put (ShortList $ Vector.toList pps) >> put (ShortList conns)
    get = liftM2 GameState (liftM (Vector.fromList . fromShortList) get) (liftM fromShortList get)
instance Binary PlacedPiece where
    put (PlacedPiece ppos p) = put ppos >> put p
    get = liftM2 PlacedPiece get get
instance Binary Piece where
    put (Block patt) = put (0::Word8) >> put (ShortList patt)
    put (Pivot arms) = put (1::Word8) >> put (ShortList arms)
    put (Hook arm stiffness) = put (2::Word8) >> put arm >> put stiffness
    put (Wrench mom) = put (3::Word8) >> put mom
    put Ball = put (4::Word8)
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> liftM (Block . fromShortList) get
	    1 -> liftM (Pivot . fromShortList) get
	    2 -> liftM2 Hook get get
	    3 -> liftM Wrench get
	    4 -> return Ball
instance Binary Connection where
    put (Connection (ri,rp) (ei,ep) l) = putPackedInt ri >> put rp >> putPackedInt ei >> put ep >> put l
    get = do
	ri <- getPackedInt
	rp <- get
	ei <- getPackedInt
	ep <- get
	l <- get
	return $ Connection (ri,rp) (ei,ep) l
instance Binary Link where
    put (Free p) = put (0::Word8) >> put p
    put (Spring d l) = put (1::Word8) >> put d >> putPackedInt l
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> liftM Free get
	    1 -> liftM2 Spring get getPackedInt
instance Binary HookForce where
    put NullHF = put (0::Word8)
    put (TorqueHF dir) = put (1::Word8) >> putPackedInt dir
    put (PushHF v) = put (2::Word8) >> put v
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> return NullHF
	    1 -> liftM TorqueHF getPackedInt
	    2 -> liftM PushHF get
instance Binary Frame where
    put (BasicFrame s) = putPackedInt s
    get = liftM BasicFrame getPackedInt

instance Binary PlayerMove where
    put NullPM = put (0::Word8)
    put (HookPush v) = put (1::Word8) >> put v
    put (HookTorque dir) = put (2::Word8) >> putPackedInt dir
    put (WrenchPush v) = put (3::Word8) >> put v
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> return NullPM
	    1 -> liftM HookPush get
	    2 -> liftM HookTorque getPackedInt
	    3 -> liftM WrenchPush get

