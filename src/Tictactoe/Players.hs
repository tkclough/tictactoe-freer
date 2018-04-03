module Tictactoe.Players where

import Control.Applicative ((<|>))

import Data.List
import Data.Maybe

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.NonDet
import Control.Monad.Freer.Coroutine
import Control.Monad (join)

import qualified Control.Monad.Freer.State as S
import qualified Data.Vector as V
import Tictactoe.Types
import Text.Read (readMaybe)

consolePlayer :: (Members [Console String, BoardQuery, Error InputException] effs) => Eff effs Index
consolePlayer = do 
    putStr' "Enter an index 0-8: "
    input <- getLine'
    case readMaybe input :: Maybe Int of
        Nothing -> do throwError MalformedInput
                      consolePlayer
        Just x -> do if x < 0 || x > 8 
                     then do throwError OutOfBoundsIndex
                             consolePlayer 
                     else do square <- get x
                             case square of 
                                 Nothing -> return x
                                 Just _ -> do throwError PositionTaken
                                              consolePlayer


consoleGame :: IO (Maybe Mark)
consoleGame = runM
            . S.evalState (V.replicate 9 (Nothing :: Maybe Mark))
            . interpret boardMutateToState
            . interpret boardQueryToState
            . interpretM consoleStrToIO
            . interpretM errToIO
            $ game consolePlayer consolePlayer X


aiPlayer :: Members [BoardQuery, NonDet] effs => Mark -> Eff effs Index
aiPlayer X = fromJust . join . find isJust <$>  sequence
  [
    win X,
    win O
  ]

fork :: Member BoardQuery effs => Mark -> Eff effs (Maybe Index)

win ::  Member BoardQuery effs => Mark -> Eff effs (Maybe Index)
win m = do
  positions <- mapM (\i -> get i >>= return . (i,)) [0..8]
  let winValues = map (map (positions !!)) winPositions :: [[(Index, Maybe Mark)]]
      winPairs  = map (partition ((== Just m) . snd)) winValues :: [([(Index, Maybe Mark)], [(Index, Maybe Mark)])]
  return $ fst . head . snd <$> find (\(a, b) -> length a == 2) winPairs
winPositions :: [[Index]]
winPositions =
  [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6]
  ]
        -- center, emptyCorner, emptySide :: Member BoardQuery effs => Eff effs (Maybe Index)
