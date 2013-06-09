module UI where 

import Control.Monad.Trans

import GameState
import Command

class (Functor m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawPlayState :: PlayState -> m ()
    drawEditState :: EditState -> m ()
    drawAlerts :: [Alert] -> m ()
    getInput :: GameMode -> m [ Command ]

