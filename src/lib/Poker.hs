--TODO consider releasing this module as a library
module Poker where

import Control.Monad
import Control.Monad.Trans.Random.Lazy
import Data.Bool
import Data.Composition
import Data.Generics.Labels
import Data.Function
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Data.Tuple.Extra
import Lens.Micro
import System.FilePath
import System.Random
import System.Random.Shuffle

import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map,(!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

--TODO remove
import qualified Debug.Trace as Debug
import qualified System.IO.Unsafe as Unsafe
import Text.Pretty.Simple (pPrint)

{- Basic types -}

-- note that the Ord instance treats Ace as high
data Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, FromJSON, ToJSON)

-- note Ord is arbitrary
data Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, FromJSON, ToJSON)

-- note Ord is arbitrary (due to suit)
data Card = Card
    { rank :: Rank
    , suit :: Suit
    } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

fullDeck :: [Card]
fullDeck = Card <$> enumerate <*> enumerate


--TODO better word?
data Stage
    = Lobby -- as it stands, this is the initial state, and we never return to it
    | PreFlop
    | Flop Card Card Card
    | Turn Card Card Card Card
    | River Card Card Card Card Card
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
tableCards :: Stage -> [Card]
tableCards = \case
    Lobby -> []
    PreFlop -> []
    Flop c1 c2 c3 -> [c1,c2,c3]
    Turn c1 c2 c3 c4 -> [c1,c2,c3,c4]
    River c1 c2 c3 c4 c5 -> [c1,c2,c3,c4,c5]
stageName :: Stage -> Text
stageName = \case
    Lobby -> "Lobby"
    PreFlop -> "Pre-flop"
    Flop{} -> "Flop"
    Turn{} -> "Turn"
    River{} -> "River"


{- Game types -}

data Player = Player
    { name :: Text
    } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data GameState = GameState
    { players :: Map Player (Card, Card)
    , stage :: Stage
    , deck :: [Card]
    , rand :: StdGen
    } deriving (Generic, Show)

initialGameState :: IO GameState
initialGameState = do
    rand <- getStdGen
    return GameState
        { stage = Lobby
        , players = mempty
        , deck = []
        , rand
        }

-- returns Nothing iff round is complete
--TODO hmm, maybe we should start using StateT
nextStage :: GameState -> Maybe GameState
nextStage s@GameState{..} = case stage of
    Lobby -> Just GameState{stage = PreFlop, ..}
    PreFlop ->
        let (c1,s1) = drawCard s
            (c2,s2) = drawCard s1
            (c3,s3) = drawCard s2
        in  Just $ s3 {stage = Flop c1 c2 c3}
    Flop c1 c2 c3 ->
        let (c4,s') = drawCard s
        in  Just $ s' {stage = Turn c1 c2 c3 c4}
    Turn c1 c2 c3 c4 ->
        let (c5,s') = drawCard s
        in  Just $ s' {stage = River c1 c2 c3 c4 c5}
    River{} -> Nothing

drawCard :: GameState -> (Card, GameState)
drawCard = traverseOf #deck \case
    [] -> error "too many players - deck ran out"
    c:cs -> (c,cs)

newHand :: [Player] -> GameState -> GameState
newHand newPlayers GameState{..} = -- newPlayers contains those who have connected since last round
    let (cs0, r) = runRand (shuffleM fullDeck) rand
        (cs, players') = Map.mapAccum (flip f) cs0 $ (() <$ players) <> Map.fromList (zip newPlayers $ repeat ()) --TODO little bit ugly
        f () = \case
            c1 : c2 : cs' -> (cs', (c1, c2))
            _ -> error "too many players - deck ran out" --TODO rule this out by refusing connections after player 8
    in GameState
        { stage = PreFlop
        , players = players'
        , deck = cs
        , rand = r
        }

-- the state transmitted to each player
data PlayerState = PlayerState
    { pocket :: (Card, Card) --TODO is that the right word?
    , table :: [Card]
    , players :: [Player]
    , someText :: Text
    , hand :: Maybe Hand
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)

--TODO make total - don't error when player not in map
playerState :: Player -> GameState -> PlayerState
playerState p GameState{..} = PlayerState
    { pocket
    , table
    , players = Map.keys players --TODO assign an order
    , someText = "placeholder" --TODO tell user what to do (or just remove this)
    , hand
    }
    where
        pocket@(p1,p2) = fromMaybe (error $ show p <> " not part of game state") $ players !? p
        table = tableCards stage
        hand = guard (not $ null table) $> classifyHand (p1 : p2 : table)


{- Hands -}

data Hand
    = HighCard Rank Rank Rank Rank Rank -- highest first
    | Pair Rank Rank Rank Rank -- rank, kickers in order
    | TwoPair Rank Rank Rank -- high pair, low pair, kicker
    | ThreeOfAKind Rank Rank Rank -- rank, kickers in order
    | Straight Rank -- highest
    | Flush Rank Rank Rank Rank Rank -- highest first
    | FullHouse Rank Rank -- three, two
    | FourOfAKind Rank Rank -- rank, kicker
    | StraightFlush Rank -- highest
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

--TODO more info (mention kickers)?
--TODO highlight cards in client?
    -- use different colours for each player at showdown?
showHand :: Hand -> Text
showHand = \case
    HighCard{} -> "High Card"
    Pair{} -> "Pair"
    TwoPair{} -> "Two Pairs"
    ThreeOfAKind{} -> "Three of a Kind"
    Straight{} -> "Straight"
    Flush{} -> "Flush"
    FullHouse{} -> "Full House"
    FourOfAKind{} -> "Four of a Kind"
    StraightFlush Ace -> "Royal Flush"
    StraightFlush{} -> "Straight Flush"

-- form best possible five-card hand
-- input list can have any length but function is optimised for around 7
-- can error if input list has length < 5
--TODO QuickCheck
classifyHand :: [Card] -> Hand
classifyHand cs0
    | Just r <- listToMaybe $ sortOn Down $ mapMaybe (highestStraight . map (^. #rank) . NE.toList) $
            takeWhile ((>= 5) . length) suits
        = StraightFlush r
    | ((n,r),cs') : _ <- ranks
        , n >= 4
        = list1 (FourOfAKind r) $ map (^. #rank) $ removeSub (NE.toList cs') cs
    | ((n1,r1),_) : ((n2,r2),_) : _ <- ranks
        , n1 >= 3
        , n2 >= 2
        = FullHouse r1 r2
    | (c1:c2:c3:c4:c5:_) : _ <- map (map (^. #rank) . NE.toList) suits
        = Flush c1 c2 c3 c4 c5
    | Just r <- highestStraight $ map (^. #rank) cs
        = Straight r
    | ((n,r),cs') : _ <- ranks
        , n >= 3
        = list2 (ThreeOfAKind r) $ map (^. #rank) $ removeSub (NE.toList cs') cs
    | ((n1,r1),cs1) : ((n2,r2),cs2) : _ <- ranks
        , n1 >= 2
        , n2 >= 2
        = list1 (TwoPair r1 r2) $ map (^. #rank) $ removeSub (NE.toList $ cs1 <> cs2) cs
    | ((n,r),cs') : _ <- ranks
        , n >= 2
        = list3 (Pair r) $ map (^. #rank) $ removeSub (NE.toList cs') cs
    | otherwise = list5 HighCard $ map (^. #rank) cs
    where
        cs = trace $ sortOn Down cs0 --descending list
        suits = classifyOn (^. #suit) cs
        ranks = sortOn (Down . fst) $ map ((NE.length &&& ((^. #rank) . NE.head)) &&& NE.sortBy (comparing Down)) $
            classifyOn (^. #rank) cs
        list1 = takeOneAndPassTo const
        list2 = takeOneAndPassTo list1
        list3 = takeOneAndPassTo list2
        list4 = takeOneAndPassTo list3
        list5 = takeOneAndPassTo list4
        takeOneAndPassTo x f = list (error "not enough cards in hand") $ x . f

-- given descending lists, return the difference
--TODO more precise description
removeSub :: Ord a => [a] -> [a] -> [a]
removeSub _ [] = []
removeSub [] ys = ys
removeSub (x:xs) (y:ys) = case compare x y of
    LT -> y : removeSub (x:xs) ys
    EQ -> removeSub xs ys
    GT -> error "first list is not a subsequence of second"

-- given descending list of ranks,
-- returns the highest card of a straight if there is one
highestStraight :: [Rank] -> Maybe Rank
highestStraight [] = Nothing
highestStraight (r:rs) =
    --TODO turn this into an explicit fold?
    let go :: Int -> Rank -> Rank -> [Rank] -> Maybe Rank
        go 5 candidate _ = const $ Just candidate
        go n candidate expected = if candidate < Five then const Nothing else \case
            [] -> guard (n == 4 && r == Ace && candidate == Five) $> Five -- special case for 5-Ace straight
            c:cs -> case compare c expected of
                EQ -> go (n+1) candidate (pred expected) cs -- found what we wanted - increment count
                LT -> go 1 c (pred c) cs -- chain broken - c is the new candidate
                GT -> go n candidate expected cs -- c
    in  go 1 r (pred r) rs


{- Util -}

--TODO specify precisely the guarantee about preserving order that we use in 'classifyHand'
--TODO return the resullt of f along with the list
classifyOn :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
classifyOn f = \case
    [] -> []
    (x : xs) -> (x :| ys) : classifyOn f zs
        where (ys, zs) = partition (((==) `on` f) x) xs

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

flipOrder :: Ordering -> Ordering
flipOrder = \case
    GT -> LT
    LT -> GT
    EQ -> EQ

{-# NOINLINE trace #-}
trace :: Show a => a -> a
trace x = Unsafe.unsafePerformIO $ pPrint x >> return x
