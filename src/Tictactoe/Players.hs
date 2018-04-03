module Tictactoe.Players where

import Control.Applicative ((<|>))

import Data.List
import Data.Maybe

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.NonDet
import Control.Monad.Freer.Coroutine
import Control.Monad (join, filterM)

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


aiConsoleGame :: IO (Maybe Mark)
aiConsoleGame = join
              . runM 
              . S.evalState (V.replicate 9 (Nothing :: Maybe Mark))
              . interpret boardMutateToState
              . interpret boardQueryToState
              . makeChoiceA
              . interpretM consoleStrToIO
              . interpretM errToIO 
              $ game (aiPlayer X) consolePlayer X
              
            
-- |Implementation of perfect AI strategy.
aiPlayer :: Members [BoardQuery, NonDet] effs => Mark -> Eff effs Index
aiPlayer m = fromJust 
           . join 
           . find isJust 
           <$> sequence
            [
              win m,        -- Place third in a row
              win m',       -- Block opponent's third in a row
              -- fork m,       -- Create opportunity where opponent has two threats
              -- fork m',      -- Block opponent's opportunity to create three in a row
              center,       -- Place in center if first move
              oppCorner m,  -- Place opposite to opponent corner move
              emptyCorner,  -- Place in a corner square
              emptySide     -- Place in empty middle square on any side
            ]
  where m' = if m == X then O else X

-- |Create an opportunity where the player has two threats to win (two 
--  non-blocked lines of 2).
fork :: Member BoardQuery effs => Mark -> Eff effs (Maybe Index)
fork m = do 
  viable <- filterM (\i -> isNothing <$> get i) [0..8]
  undefined


-- |A player marks the center.
center :: Member BoardQuery effs => Eff effs (Maybe Index)
center = do 
  cent <- get 4
  if isJust cent 
    then return Nothing 
    else return (Just 4) 

-- |If the opponent is in the corner, the player plays the opposite corner.
oppCorner :: Member BoardQuery effs => Mark -> Eff effs (Maybe Index)
oppCorner m = do 
  positions <- mapM (\i -> get i >>= return . (i,)) [0, 2, 6, 8]
  let os = filter (\(i, k) -> case k of 
                    Just m' -> True
                    _ -> False) positions
  if length os == 0 
    then return Nothing 
    else return . Just . opp . fst . head $ os
    where m' = if (m == X) then O else X

          opp 0 = 6
          opp 2 = 8
          opp 6 = 0
          opp 8 = 2

-- |The player plays in a corner square.
emptyCorner :: Member BoardQuery effs => Eff effs (Maybe Index)
emptyCorner = do 
  positions <- mapM (\i -> get i >>= return . (i,)) [0, 2, 6, 8]
  let sol = fst <$> 
            find (\(i, x) -> not (isJust x)) positions :: Maybe Index
  return sol

-- |The player plays in a middle square of any of the 4 sides.
emptySide :: Member BoardQuery effs => Eff effs (Maybe Index)
emptySide = do 
  positions <- mapM (\i -> get i >>= return . (i,)) [1, 3, 5, 7]
  let sol = fst <$> 
            find (\(i, x) -> not (isJust x)) positions :: Maybe Index
  return sol

win ::  Member BoardQuery effs => Mark -> Eff effs (Maybe Index)
win m = do
  positions <- mapM (\i -> get i >>= return . (i,)) [0..8]
  let winValues = map (map (positions !!)) winPositions :: [[(Index, Maybe Mark)]]
      winPairs  = map (partition ((== Just m) . snd)) winValues :: [([(Index, Maybe Mark)], [(Index, Maybe Mark)])]
      winPairs' = filter (\(a, b) -> all isNothing . map snd $ b) winPairs
  return $ fst . head . snd <$> find (\(a, b) -> length a == 2) winPairs'
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
