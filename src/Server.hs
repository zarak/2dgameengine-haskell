{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (
) where

import Control.Exception.Safe (bracket)
import Control.Monad (unless)
import Data.Attoparsec.Text
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SocketBS
import qualified Network.Socket.ByteString.Lazy as SocketLBS
import System.IO
import Control.Concurrent (forkIO)
import Constants
import Data.Serialize
import GHC.Generics
import qualified Network.WebSockets as WS

data MyData = Foo Int | Bar String
  deriving (Generic)

instance Serialize MyData

-- Now use encode, decode to serialize/deserialize from Bytestrings
--
type Client = (Text, WS.Connection)
--
newServerState :: [Client]
newServerState = []

-- Network.Websockets.ServerApp
