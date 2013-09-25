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
    askYesNo :: String -> m ()
    getInput :: InputMode -> m [ Command ]

data PlayState = PlayState
	{ psCurrentState::GameState
	, psLastAlerts::[Alert]
	, wrenchSelected::Bool
	, psSolved::Bool
	, psSwitchMode::Maybe GameMode
	, psGameStateMoveStack::[(GameState, PlayerMove)]
	, psUndoneStack::[(GameState, PlayerMove)]
	}
    deriving (Eq, Ord, Show)
newPlayState st = PlayState st [] False False Nothing [] []

data EditState = EditState {
	esGameStateStack::[GameState], esUndoneStack::[GameState]
	, selectedPiece::Maybe PieceIdx, selectedPos::HexPos
	, lastModPos::HexPos, esSwitchMode::Maybe GameMode}
    deriving (Eq, Ord, Show)

data InputMode = IMEdit | IMPlay | IMConfirm
    deriving (Eq, Ord, Show, Read)
