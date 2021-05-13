module Server (
  runConn,
  pos,
  main,
) where

import Control.Exception.Safe (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SocketBS
import qualified Network.Socket.ByteString.Lazy as SocketLBS
import System.IO
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.Read as T

pos = [(0, 0), (100, 100)]

forChunks_ ::
  IO chunk ->
  (chunk -> Bool) ->
  (chunk -> IO x) ->
  IO ()
forChunks_ getChunk isEnd f = continue
 where
  continue = do
    chunk <- getChunk
    unless (isEnd chunk) $ do
      _ <- f chunk
      continue

withSocket :: S.Family -> S.SocketType -> S.ProtocolNumber -> (S.Socket -> IO a) -> IO a
withSocket addrFamily socketType protocol = bracket open close
 where
  open = S.socket addrFamily socketType protocol
  close s = S.gracefulClose s 1000

main :: IO ()
main = withSocket S.AF_INET S.Stream 0 $ \sock -> do
  S.setSocketOption sock S.ReuseAddr 1
  let hints = S.defaultHints{S.addrFlags = [S.AI_NUMERICHOST, S.AI_NUMERICSERV], S.addrSocketType = S.Stream}
  addr : _ <- S.getAddrInfo (Just hints) (Just "192.168.100.23") (Just "5000")
  S.bind sock (S.addrAddress addr)
  S.listen sock 2
  mainLoop sock

mainLoop :: S.Socket -> IO ()
mainLoop sock = do
  conn <- S.accept sock
  runConn conn
  mainLoop sock

runConn :: (S.Socket, S.SockAddr) -> IO ()
runConn (sock, addr) = do
  msg <- SocketBS.recv sock 1024
  let clientPos = T.double $ T.decodeUtf8 msg
  case clientPos of
    Left e -> print $ "Invalid position" <> e
    Right d -> print $ "Received position " <> show d
  unless (BS.null msg) $ do
    SocketBS.sendAll sock $ T.encodeUtf8 $ T.pack $ show $ head pos
    runConn (sock, addr)
