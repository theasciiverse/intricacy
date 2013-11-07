module InputMode where

data InputMode = IMEdit | IMPlay | IMReplay | IMMeta | IMTextInput
    deriving (Eq, Ord, Show, Read)
