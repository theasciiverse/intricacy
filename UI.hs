module UI where 

import Control.Monad.Trans

import GameState
import Physics
import Command

class (Functor m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawPlayState :: PlayState -> m ()
    drawEditState :: EditState -> m ()
    drawAlerts :: [Alert] -> m ()
    getInput :: InputMode -> m [ Command ]

data PlayState = PlayState {psGameStateStack::[GameState], psLastAlerts::[Alert]
	, wrenchSelected::Bool, psSwitchMode::Maybe GameMode }
    deriving (Eq, Ord, Show)
data EditState = EditState {esGameStateStack::[GameState]
	, selectedPiece::Maybe PieceIdx, selectedPos::HexPos,
	lastModPos::HexPos, esSwitchMode::Maybe GameMode}
    deriving (Eq, Ord, Show)

data InputMode = IMEdit | IMPlay | IMConfirm
