{-#LANGUAGE OverloadedStrings#-}

module Server
  (
      Server(..)
    , HTTPRequest(..)
    , HTTPResponse(..)
    , sendResponse
    , Handler
    , Route
    , Router 
    , route 
    , server
    , runServer
    
  ) where

 
import Network.Socket
import qualified Network.Socket.ByteString as NB
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
import Data.Attoparsec.ByteString.Char8 (parse, feed, IResult (Done) )
import Data.ByteString.Char8 (pack, empty)
import qualified Data.ByteString as DB
import Data.Map.Strict as M

import HTTPRequest

type Route = (DB.ByteString, DB.ByteString)
type Handler = (HTTPRequest -> IO HTTPResponse)
type Router = Map Route Handler

data Server = Server {
  sock     :: Socket,
  router   :: Router
                     }

server :: [(Route, Handler)] -> IO Server
server routes = do
  sock <- ( socket AF_INET Stream 0)
  return $ Server (sock) (fromList routes)

runServer :: IO Server -> PortNumber -> IO ()
runServer serv port = do
  server <- serv 
  setSocketOption (sock server) ReuseAddr 1
  bind (sock server) (SockAddrInet port iNADDR_ANY)
  listen (sock server) 2

  mainLoop server


route :: Route -> Handler -> (Route, Handler)
route r h = (r, h)


mainLoop :: Server -> IO ()
mainLoop serv  = do
  conn <- accept (sock serv)
  forkIO (runConn serv conn )
  mainLoop serv
 
runConn :: Server -> (Socket, SockAddr) -> IO ()
runConn serv (sock, _) = do
  
    contents <- receive sock ""

    let req = feed (parse request contents) DB.empty
    case req of
      Done _ result -> do
        handleRequest result serv sock   
      _ -> DB.putStrLn $ "Not done :" `DB.append` contents
    close sock

receive :: Socket -> DB.ByteString -> IO DB.ByteString
receive s bs = do

  case bs of
    "" -> do
      received <- NB.recv s 4096
      case ("\r\n" `DB.findSubstring` received) of
        Just yes -> return received
        Nothing -> do
          receive s received
    fullStr -> do
      let lastChar = DB.last bs
      received <- NB.recv s 4096
  
      case ("\r\n" `DB.findSubstring` (DB.cons lastChar received)) of
        Just yes -> return $ DB.append bs received
        Nothing -> do
          receive s (DB.append bs received)
  
handleRequest :: HTTPRequest -> Server -> Socket -> IO () 
handleRequest req serv sock = do 

  let r = (method req, uri req)
  case (M.lookup r $ router serv) of
    Just handler -> do
      res <- handler req
      let toSend = readResponse res
      NB.sendAll sock $ toSend
    Nothing -> NB.sendAll sock $ readResponse $ sendResponse "404" "Not Found" $ "Cannot " `DB.append` (method req) `DB.append` " " `DB.append` (uri req)

