module Main where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Random.Lazy
import Control.Monad.Trans.State.Strict
import Control.Monad.STM
import Data.Aeson
import Data.Char
import Data.Composition
import Data.Function
import Data.Functor
import Data.Generics.Labels
import Data.Maybe
import Data.Monoid
import Data.Tuple.Extra
import GHC.OverloadedLabels
import Lens.Micro
import Lens.Micro.Extras
import Options.Generic
import System.IO

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Text.Pretty.Simple (pPrint)

import Poker

data Opts = Opts
    { address :: String
    , port :: Int
    } deriving Generic
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{shortNameModifier = firstLetter}

data ServerState = ServerState
    { clients :: Map Player WS.Connection
    , game :: GameState
    } deriving Generic

-- those who have connected during the current hand
type PlayerQueue = TVar [(Player, WS.Connection)]

--TODO allow save/load game state from JSON (would need to wait for correct no. of players)
main :: IO ()
main = do
    T.putStrLn "Server starting..."
    Opts{..} <- getRecord "Jemima server"
    queue <- newTVarIO []
    s <- initialGameState
    let ss = ServerState
            { clients = []
            , game = s
            }
    void $ forkIO $ WS.runServer address port $ application queue
    T.putStrLn "Server started."
    evalStateT (mainLoop queue) ss

application :: PlayerQueue -> WS.ServerApp
application queue pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) do
        name <- WS.receiveData conn
        let player = Player name
        --TODO disallow duplicated names, maybe whitespace, max length
        atomically $ modifyTVar' queue ((player, conn) :)
        T.putStrLn $ name <> " joined"
        forever $ threadDelay maxBound
        --TODO work out why we can't let this thread die after we've added the connection to queue
            -- this is key to simplifying client

-- --TODO what to do if a player drops? probably whatever we would do if they went bankrupt...
-- disconnect = T.putStrLn $ name <> " disconnected"
-- talk player conn `finally` disconnect
-- --TODO bit pointless now really (indeed client never sends messages)
--     -- this is really a placeholder for whatever we do to await messages from clients
-- talk :: Player -> WS.Connection -> IO ()
-- talk p c = forever do
--     msg <- WS.receiveData c
--     T.putStrLn (view #name p <> ": " <> msg)

mainLoop :: PlayerQueue -> StateT ServerState IO ()
mainLoop queue = forever do --TODO output text telling current state, and to press enter
    s <- get
    let sg = s ^. #game

    --TODO this doesn't work well when another thread writes to the terminal
        -- we could use a terminal library, or just use GTK for the server as well
        -- or use a lock for writing to the terminal
    liftIO do
        T.putStr $ stageName (sg ^. #stage) <> ": press enter to proceed"
        hFlush stdout
        void T.getLine -- wait for input

    s'@ServerState{..} <- case view #stage sg of
            Lobby -> liftIO $ new s
            _ -> case nextStage sg of
                Just sg' -> return $ s{game = sg'}
                Nothing -> liftIO $ new s
    put s'
    forM_ (Map.toList clients) $ \(p,c) ->
        liftIO $ WS.sendTextData c $ encode $ playerState p game

    where
        -- add clients from queue to game, and begin new hand
        new ServerState{..} = do
            newClients <- atomically $ stateTVar queue \q -> (Map.fromList q, [])
            let clients' = Map.unionWith (error "duplicate player name") clients newClients --TODO
                newPlayers = Map.keys newClients
            unless (null newPlayers) do
                T.putStrLn "New players:"
                forM_ newPlayers \Player{..} ->
                    T.putStrLn $ "  " <> name
            return $ ServerState
                { game = newHand newPlayers game
                , clients = clients'
                }
