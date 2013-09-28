module Main where

import Data.BinaryCom as BC
import Network.Fancy

import Control.Monad
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Binary

import Protocol
import Metagame
import BinaryInstances

port = 27001 -- 27001 == ('i'<<8) + 'y'
main = withStream (IP "localhost" port) handler

handler :: Handle -> IO ()
handler handle = do
    --let testReq = ClientRequest protocolVersion (Just $ Auth "ZGZ" "secret") Authenticate
    let testReq = ClientRequest protocolVersion (Just $ Auth "../" "secret") Authenticate
    --com <- BC.binaryCom handle
    --BC.send testReq
    --BC.receive com >>= print

    L.hPut handle $ encode testReq 
    hFlush handle
    response <- decode `liftM` L.hGetContents handle
    print (response :: ServerResponse)

