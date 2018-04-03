module Tictactoe.Types where

import Prelude hiding (replicate)
import Control.Monad.Freer 
import Control.Monad.Freer.Error
import qualified Control.Monad.Freer.Reader as R
import qualified Control.Monad.Freer.State as S
import Data.Vector (Vector)
import Control.Monad ((<=<))
import qualified Data.Vector as V

-- |A `Mark` represents the value any square can take - X or O.
data Mark = X | O deriving (Show, Eq)

-- |An `Index` is an integer in the range [0, 8].
type Index = Int

-- |`BoardQuery` represents the effect for getting a specific index.
data BoardQuery r where 
    Get :: Index -> BoardQuery (Maybe Mark)

boardQueryToReader :: Member (R.Reader (Vector (Maybe Mark))) effs => BoardQuery ~> Eff effs
boardQueryToReader (Get i) = R.asks (V.! i)

boardQueryToState :: Member (S.State (Vector (Maybe Mark))) effs => BoardQuery ~> Eff effs 
boardQueryToState (Get i) = S.gets (V.! i)

data InputException = 
    MalformedInput
    | OutOfBoundsIndex
    | PositionTaken
    deriving (Eq)

instance Show InputException where 
    show = \case
        MalformedInput -> "Invalid input."
        OutOfBoundsIndex -> "Invalid index."
        PositionTaken -> "Position taken."

-- |`BoardMutate` represents the effect for setting a position.
data BoardMutate r where 
    Set :: Mark -> Index -> BoardMutate ()

boardMutateToState :: Member (S.State (Vector (Maybe Mark))) effs => BoardMutate ~> Eff effs 
boardMutateToState (Set m i) = S.modify (V.// [(i, Just m)])

get :: Member BoardQuery effs => Index -> Eff effs (Maybe Mark)
get = send . Get 

set :: Member BoardMutate effs => Mark -> Index -> Eff effs ()
set mark = send . Set mark  

-- |`Console t` represents effects involving console I/O
data Console t r where 
    PutStr :: t -> Console t ()
    GetLine :: Console t t 

putStr' :: Member (Console String) effs => String -> Eff effs ()
putStr' = send . PutStr 

getLine' :: Member (Console String) effs => Eff effs String
getLine' = send GetLine

consoleStrToIO :: Console String ~> IO 
consoleStrToIO (PutStr x) = putStr x 
consoleStrToIO GetLine = getLine

errToIO :: Error InputException ~> IO 
errToIO (Error s) = print s >> return undefined

emptyBoard :: Vector (Maybe Mark)
emptyBoard = V.replicate 9 Nothing


game :: Members [BoardQuery, BoardMutate, Console String] effs => 
    Eff effs Index -> Eff effs Index -> Mark -> Eff effs (Maybe Mark)
game x o turn = do 
    gs <- status
    renderBoard 
    case gs of 
        Win m -> do putStr' (show m ++ " wins\n") 
                    return $ Just m 
        Tie -> do putStr' "Tie" 
                  return Nothing 
        InProgress -> 
            if turn == X 
                then do 
                    putStr' "X's turn\n"
                    i <- x
                    set X i 
                    game x o O 
                else do 
                    putStr' "O's turn\n"
                    i <- o
                    set O i 
                    game x o X


renderBoard :: Members [BoardQuery, Console String] effs => Eff effs ()
renderBoard = do 
    b' <- mapM get [0..8]
    let b = map render b'
    putStr' $ (b !! 0) ++ " | " ++ (b !! 1) ++ " | " ++ (b !! 2) ++ "\n"
    putStr' $ "-----------\n"
    putStr' $ (b !! 3) ++ " | " ++ (b !! 4) ++ " | " ++ (b !! 5) ++ "\n"
    putStr' $ "-----------\n"
    putStr' $ (b !! 6) ++ " | " ++ (b !! 7) ++ " | " ++ (b !! 8) ++ "\n"
    where 
    render :: Maybe Mark -> String 
    render (Just X) = "X"
    render (Just O) = "O"
    render Nothing = " "

data GameStatus = Win Mark 
                | Tie 
                | InProgress
                deriving (Show, Eq)

status :: Member BoardQuery effs => Eff effs GameStatus
status = do
    x <- hasWon X 
    o <- hasWon O
    p <- inProgress
    if x 
        then return $ Win X
    else if o
        then return $ Win O 
    else if p 
        then return InProgress 
        else return Tie
    where hasWon m = any (== True) <$> mapM (\row -> all (== Just m) <$> mapM get row) positions
          positions = [
            [0, 1, 2],
            [3, 4, 5],
            [6, 7, 8],
            [0, 3, 6],
            [1, 4, 7],
            [2, 5, 8],
            [0, 4, 8],
            [2, 4, 6]]
          inProgress = any (== Nothing) <$> mapM get [0..8]



{-
runM
. runReader ((Just X) `V.cons` V.replicate 9 Nothing)
. interpret boardQueryToReader
. interpretM errToIO
. interpretM consoleStrToIO
$ consolePlayer
-}