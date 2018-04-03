{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGe DeriveGeneric #-}

module Tictactoe.GUI where

import Data.Aeson hiding (json)
import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy.IO as T
import GHC.Generics
import Control.Monad.Freer 
import qualified Tictactoe.Types as Ty
import Data.HashMap.Strict (fromList)

{-
app :: ActionM ()
app = do 
    send . get "/" $ do 
        content <-  send $ liftIO (T.readFile "static/board.html")
        send $ html content
    send . post "/" $ do
        ix <- send $ (jsonData :: ActionM Int)
        send . liftIO $ print body
        send . liftIO $ T.putStrLn "posted"
        m <- Ty.get ix 
        case m of 
            Nothing -> do 
                Ty.set m ix 
                text "Succeeded"
            Just _ -> do 
                text "Failed"

                -}

postInfo :: Members [ActionM, Ty.BoardQuery, Ty.BoardMutate] effs => Eff effs ()
postInfo = do 
    ix <- send (jsonData :: ActionM Int)
    m <- Ty.get ix 
    case m of 
        Nothing -> do 
            Ty.set Ty.X ix 
            send (text "succeeded")
        Just _ -> do 
            send (text "failed")

getContent :: ActionM ()
getContent = liftIO (T.readFile "static/board.html") >>= html

getMove :: Members [Ty.BoardQuery, Ty.BoardMutate, ActionM] effs => Eff effs Ty.Index 
getMove = do 
    ix <- send jsonData
    p <- Ty.get ix 
    case p of 
        Nothing -> return ix 
        Just _ -> getMove

instance ToJSON (Ty.BoardMutate r) where 
    toJSON (Ty.Set m i) = case m of 
        Ty.X -> Object $ fromList [("mark", String "X"), ("index", toJSON i)]
        Ty.O -> Object $ fromList [("mark", String "O"), ("index", toJSON i)]


boardMutateToActionM :: Member ActionM effs => Ty.BoardMutate a -> Eff effs a
boardMutateToActionM s@(Ty.Set m i) = go s
    where go :: Member ActionM effs => Ty.BoardMutate () -> Eff effs ()
          go s = send (json s)

