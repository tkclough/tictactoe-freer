module Main where

import Tictactoe.Types
import Tictactoe.Players
import Control.Monad.Freer

main :: IO ()
main = consoleGame >> return  ()
