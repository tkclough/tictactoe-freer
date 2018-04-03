{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty
import Test.Tasty.HUnit

import Tictactoe.Types as T
import Tictactoe.Players
import Control.Monad.Freer
import Control.Monad.Freer.State as S
import Control.Monad.Freer.Error as E

import Language.Haskell.TH 
import Control.Lens


data MockState = MockState {
    _cin :: [String],
    _cout :: [String],
    _cerr :: [InputException],
    _board :: [Maybe Mark]
} deriving (Show, Eq)
makeLenses ''MockState

mockConsole :: Member (State MockState) effs => Console String ~> Eff effs
mockConsole (PutStr s) = modify (over cout (++ [s]))
mockConsole GetLine = do (l, ls) <- ((,) <$> head <*> tail) <$> gets (view cin )
                         modify (over cin (const ls))
                         return l

mockBoardQuery :: Member (State MockState) effs => BoardQuery ~> Eff effs 
mockBoardQuery (T.Get i) = gets ((!! i) <$> view board)

mockBoardMutate :: Member (State MockState) effs => BoardMutate ~> Eff effs 
mockBoardMutate (T.Set x i) = modify (over board (\b -> take i b ++ [Just x] ++ drop (i + 1) b))

mockError :: Member (State MockState) effs => Error InputException ~> Eff effs 
mockError (Error e) = modify (over cerr (++ [e])) >> return undefined

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testGroup "Unit Tests" [consolePlayerTests, statusTests] ]

consolePlayerTests :: TestTree
consolePlayerTests =
    testGroup "Console Player tests"
        [ testCase "Valid index returns index" $
          let ms = MockState ["5"] [] [] (replicate 9 Nothing)
              res = run 
                    . evalState ms 
                    . interpret mockBoardQuery 
                    . interpret mockError
                    . interpret mockConsole $ consolePlayer in 
                res @?= 5,
          testCase "Malformed index prints error" $
          let ms = MockState ["foo", "5"] [] [] (replicate 9 Nothing)
              (MockState _ _ err _) = run 
                                    . execState ms 
                                    . interpret mockError 
                                    . interpret mockBoardQuery 
                                    . interpret mockConsole $ consolePlayer in 
                (err !! 0) @?= MalformedInput,
          testCase "Negative index prints error" $
          let ms = MockState ["-1", "5"] [] [] (replicate 9 Nothing)
              (MockState _ _ err _) = run 
                                    . execState ms 
                                    . interpret mockError
                                    . interpret mockBoardQuery 
                                    . interpret mockConsole $ consolePlayer in 
                (err !! 0) @?= OutOfBoundsIndex,
          testCase "Index > 8 index prints error" $
          let ms = MockState ["20", "5"] [] [] (replicate 9 Nothing)
              (MockState _ _ err _) = run 
                                    . execState ms
                                    . interpret mockError 
                                    . interpret mockBoardQuery 
                                    . interpret mockConsole $ consolePlayer in 
                (err !! 0) @?= OutOfBoundsIndex,
          testCase "Taken position prints error" $
          let ms = MockState ["0", "5"] [] [] (Just T.X : (replicate 8 Nothing))
              (MockState _ _ err _) = run 
                                    . execState ms 
                                    . interpret mockError
                                    . interpret mockBoardQuery 
                                    . interpret mockConsole $ consolePlayer in 
                (err !! 0) @?= PositionTaken
          ]

statusTests :: TestTree
statusTests = testGroup "Status Tests"
              [
                testCase "Status on X win returns correct result" $
                let ms = MockState [] [] [] ([Just T.X, Just T.X, Just T.X] ++ replicate 6 Nothing)
                    stat = run
                      . evalState ms
                      . interpret mockBoardQuery
                      $ status in
                  stat @?= Win X,
                testCase "Status on O win returns correct result" $
                let ms = MockState [] [] [] (replicate 3 Nothing ++ replicate 3 (Just T.O) ++ replicate 3 Nothing)
                    stat = run
                      . evalState ms
                      . interpret mockBoardQuery
                      $ status in
                  stat @?= Win O,
                testCase "Tie board returns correct result" $
                let ms = MockState [] [] [] (map Just [T.X, T.X, T.O, T.O, T.O, T.X, T.X, T.X, T.O])
                    stat = run . evalState ms . interpret mockBoardQuery $ status in
                  stat @?= Tie,
                testCase "In progress returns correct result" $
                let ms = MockState [] [] [] (replicate 9 Nothing)
                    stat = run . evalState ms . interpret mockBoardQuery $ status in
                  stat @?= InProgress
              ]
