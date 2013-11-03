module ServerAddr where

import Data.List
import Data.Maybe
import Control.Applicative

data ServerAddr = ServerAddr {hostname::String, port::Int}
    deriving (Eq, Ord, Show, Read)
nullSaddr (ServerAddr host _) = null host

defaultPort=27001 -- == ('i'<<8) + 'y'
defaultServerAddr = ServerAddr "gonzales.homelinux.org" defaultPort

saddrStr (ServerAddr h p) = h ++ if p==defaultPort then "" else ':':show p

strToSaddr str =
    case elemIndex ':' str of
	Nothing -> Just $ ServerAddr str defaultPort
	Just idx -> do
	    let (addr,portstr) = splitAt idx str
	    port <- fst <$> listToMaybe (reads (drop 1 portstr))
	    return $ ServerAddr addr port
