module BinaryInstances where
import Data.Binary
import qualified Data.Vector as Vector
import Data.Word
import Control.Monad

import GameState
import Hex
import Frame
instance Binary HexVec where
    put (HexVec hx hy hz) = put hx >> put hy >> put hz
    get = liftM3 HexVec get get get
instance Binary g => Binary (PHS g) where
    put (PHS v) = put v
    get = liftM PHS get
instance Binary GameState where
    put (GameState pps conns) = put (Vector.toList pps) >> put conns
    get = liftM2 GameState (liftM Vector.fromList get) get
instance Binary PlacedPiece where
    put (PlacedPiece ppos p) = put ppos >> put p
    get = liftM2 PlacedPiece get get
instance Binary Piece where
    put (Block patt) = put (0::Word8) >> put patt
    put (Pivot arms) = put (1::Word8) >> put arms
    put (Hook arm stiffness) = put (2::Word8) >> put arm >> put stiffness
    put (Wrench mom) = put (3::Word8) >> put mom
    put Ball = put (4::Word8)
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> liftM Block get
	    1 -> liftM Pivot get
	    2 -> liftM2 Hook get get
	    3 -> liftM Wrench get
	    4 -> return Ball
instance Binary Connection where
    put (Connection r e l) = put r >> put e >> put l
    get = liftM3 Connection get get get
instance Binary Link where
    put (Free p) = put (0::Word8) >> put p
    put (Spring d l) = put (1::Word8) >> put d >> put l
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> liftM Free get
	    1 -> liftM2 Spring get get
instance Binary Stiffness where
    put = put . fromEnum
    get = liftM toEnum get
instance Binary Frame where
    put (BasicFrame s) = put s
    get = liftM BasicFrame get
