{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.Extra
import Control.Monad.Trans
import Control.Monad.Trans.Random.Lazy
import Data.Bifoldable
import Data.Bool
import Data.Char
import Data.Generics.Labels
import Data.Functor
import Data.Maybe
import System.IO
import Lens.Micro
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error

import Control.Arrow ((>>>))
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import qualified Network.WebSockets  as WS
import qualified Pipes as P
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

import GI.Gtk (
    Align(..),
    Box(..),
    Button(..),
    Dialog(..),
    Entry(..),
    Image(..),
    Label(..),
    Orientation(..),
    )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Objects.Entry
import GI.Gtk.Objects.EntryBuffer

import Poker


main :: IO ()
main = do
    chan <- newEmptyMVar
    cfg <- fromMaybe defaultConfig <$> readConfig --TODO mixing up label and contents is a bit hacky
    let env = Env cfg chan
        webEventPipe = lift (takeMVar chan) >>= \e -> --TODO bracket for when connection closes and we are blocked on MVar?
            P.yield e >> webEventPipe
    void $ run App
        { view         = view' env
        , update       = update' env
        , inputs       = [webEventPipe]
        , initialState = Startup $ StartupState Nothing cfg
        }
    where defaultConfig = Config
            { username = "username"
            , address = "address"
            , port = "port"
            }


{- Core types -}

-- global read only data created at startup
data Env = Env
    { cachedConfig :: Config
    , eventChan :: MVar Event
    } deriving Generic

data Event
    = CloseApp
    | GameUpdate PlayerState
    | StartupEvent StartupEvent
    | ConnectFailed Text -- this can occur at any point
    deriving Show

data StartupEvent
    = Connect
    | ConfigUpdate (Lens' Config Text) Text
    | ConnectSucceeded
instance Show StartupEvent where
    show = \case
        Connect -> "Connect"
        ConfigUpdate _ t -> "ConfigUpdate " ++ show t
        ConnectSucceeded -> "ConnectSucceeded"

data LobbyEvent
    = NameEntered Text
    deriving Show

data ClientState
    = InGame PlayerState
    | InLobby
    | Startup StartupState
    deriving Show

data StartupState = StartupState
    { errText :: Maybe Text
    , config :: Config
    } deriving (Show, Generic)

data Config = Config
    { username :: Text
    , address :: Text
    , port :: Text
    } deriving (Read, Show, Generic)

--TODO use Binary or something
writeConfig :: Config -> IO ()
writeConfig c = do
    t <- getTemporaryDirectory
    writeFile (t </> configFileName) $ show c --TODO catch any errors - just don't write

--TODO catch any errors - return Nothing - display failure reason
readConfig :: IO (Maybe Config)
readConfig = do
    t <- getTemporaryDirectory
    let f = t </> configFileName
    e <- doesFileExist f
    if e then
        readMaybe <$> readFile f
    else return Nothing


{- Drawing with GTK -}

view' :: Env -> ClientState -> AppView Dialog Event
view' env = \case
    Startup s -> viewStartup env s
    InLobby -> viewLobby
    InGame s -> viewGame s

-- sets some defaults that we want for all top-level windows
appView :: Widget Event -> AppView Dialog Event
appView = bin Dialog [#title := windowName, on #deleteEvent (const (True, CloseApp))]

--TODO label boxes
--TODO find way to insert text into buffer at start
viewStartup :: Env -> StartupState -> AppView Dialog Event
viewStartup Env{..} StartupState{..} = appView $ container Box
    [ #orientation := OrientationVertical ]
    [ container Box []
        [ w #address
        , w #port
        , w #username
        , widget Button
            [ #label := "Connect"
            , on #clicked $ StartupEvent Connect
            ]
        ]
    , widget Label [#label := err]
    ]
    -- <> fmap w [#address, #port, #username] --TODO understand why this doesn't work
    where
        err = --TODO also check range?
            if not $ T.all isDigit $ config ^. #port then "Port must be an integer"
            else fromMaybe "" errText
        w :: Lens' Config Text -> BoxChild Event
        w l = widget Entry
            [ onM #changed $ fmap (StartupEvent . ConfigUpdate l) . entryGetText
            , #placeholderText := (cachedConfig ^. l) --TODO PR to suggest that := should have different fixity
            ]

viewLobby :: AppView Dialog Event
viewLobby = appView $ widget Label [#label := "Waiting in lobby"] --TODO show names of connected players

viewGame :: PlayerState -> AppView Dialog Event
viewGame PlayerState{..} = appView $ container Box
    [ #orientation := OrientationVertical
    ]
    [ container Box []
        [ cardImages $ map Just $ biList pocket
        , viewHand hand
        ]
    , cardImages $ take 5 $ map Just table ++ repeat Nothing
    ]

viewHand :: Maybe Hand -> BoxChild Event
viewHand h = widget Label [#label := maybe "" showHand h]

cardImages :: [Maybe Card] -> BoxChild Event
cardImages = container Box [] . Vector.fromList . map
    (\c -> widget Image [#file := T.pack (cardImage c)])

--TODO just use SVGs once we no longer care about Ubuntu 16.04
  -- until then, we could at least use higher-res PNGs
--TODO load all assets once rather than reading from disk every time
cardImage :: Maybe Card -> FilePath
cardImage c = "rsc" </>
#ifdef USE_PNG
    "cards-png" </> c' <.> "png"
#else
    "cards" </> c' <.> "svg"
#endif
    where c' = case c of
            Nothing -> "1B"
            Just (Card r s) ->
                let r' = case r of
                        Ace -> 'A'
                        Two -> '2'
                        Three -> '3'
                        Four -> '4'
                        Five -> '5'
                        Six -> '6'
                        Seven -> '7'
                        Eight -> '8'
                        Nine -> '9'
                        Ten -> 'T'
                        Jack -> 'J'
                        Queen -> 'Q'
                        King -> 'K'
                    s' = case s of
                        Clubs -> 'C'
                        Diamonds -> 'D'
                        Hearts -> 'H'
                        Spades -> 'S'
                in  r' : pure s'


{- State update -}

update' :: Env ->  ClientState -> Event -> Transition ClientState Event
update' env s e
    | CloseApp <- e =
        Exit
    | ConnectFailed t <- e =
        Transition (Startup $ StartupState (Just t) cfg) $ T.putStrLn t $> Nothing
    | Startup ss <- s,  StartupEvent se <- e =
        updateStartup env ss se
    | InLobby <- s, GameUpdate sg <- e =
        Transition (InGame sg) $ return Nothing
    | InGame _ <- s, GameUpdate sg <- e =
        updateGame sg
    | otherwise =
        Transition s do
            T.putStrLn "Warning: unexpected event in this state: "
            pPrint (s, e)
            return Nothing
    where cfg = case s of
            Startup ss -> ss ^. #config
            _ -> env ^. #cachedConfig

updateGame :: PlayerState -> Transition ClientState Event
updateGame s = Transition (InGame s) $ return Nothing

updateStartup :: Env -> StartupState -> StartupEvent -> Transition ClientState Event
updateStartup Env{..} s@StartupState{..} = \case
    ConfigUpdate l t -> Transition (Startup $ StartupState errText $ f l t) (return Nothing)
    Connect -> Transition (Startup s) $ startWebApp eventChan config $> Nothing
    ConnectSucceeded -> Transition InLobby $ return Nothing
    where -- use value from initial (cached) config if text is empty
        f :: Lens' Config Text -> Text -> Config
        f l t = config & bool (l .~ t) (l .~ (cachedConfig ^. l)) (T.null t)

{- Web sockets -}

-- TODO handle failure to connect (return Nothing?)
startWebApp :: MVar Event -> Config -> IO ()
startWebApp chan cfg@Config{..} = void $ forkIO do
    writeConfig cfg --TODO only write if connection is successful?
    case readMaybe $ T.unpack port of
        Nothing -> return ()
        Just p -> handle
            (\(e :: IOException) ->
                putMVar chan $ ConnectFailed $ ("Connection error: " <>) $ T.pack $ ioeGetErrorString e)
            (void $ WS.runClient (T.unpack address) p "/" $ webApp username chan)

--TODO there's almost certainly a more direct Pipes-y way to do this, without the explicit MVar
webApp :: Text -> MVar Event -> WS.ClientApp ()
webApp n m conn = do
    putMVar m $ StartupEvent ConnectSucceeded
    WS.sendTextData conn n
    forever do
        msg <- WS.receiveData conn
        case decode msg :: Maybe PlayerState of
            Nothing -> do
                BS.putStrLn msg
                T.putStrLn "Failed to decode game state - exiting"
                exitFailure
            Just s -> putMVar m $ GameUpdate s


{- Constants -}

windowName :: Text
windowName = "Jemima"

configFileName :: FilePath
configFileName = "jemima-config"
