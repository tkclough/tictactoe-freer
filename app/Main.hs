{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Main where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Tictactoe.Types
import Control.Monad.Freer hiding (run)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Control.Monad.Freer.State (evalState)

data Position = Position
  { index         :: Index
  , mark          :: Maybe Mark 
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mark)
$(deriveJSON defaultOptions ''Position)

type API  =  "query" :> Capture "index" Index :> Get '[JSON] Position
        :<|> "set" :> ReqBody '[JSON] Position :> Post '[JSON] ()

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =   queryBoard
     :<|> setBoard
    where 
         queryBoard :: Index -> Handler Position
         queryBoard ix = runM
                         . evalState (V.replicate 9 (Just X :: Maybe Mark))
                         . interpret boardQueryToState
                         $ queryBoardEff ix

         setBoard :: Position -> Handler ()
         setBoard pos = runM 
                       . evalState (V.replicate 9 (Nothing :: Maybe Mark))
                       . interpret boardMutateToState
                       $ setBoardEff pos

queryBoardEff :: Members [BoardQuery, Handler] effs => Index -> Eff effs Position
queryBoardEff ix = do 
    m <- get ix
    return $ Position ix m

setBoardEff :: Members [BoardMutate, Handler] effs => 
                Position -> Eff effs ()
setBoardEff (Position ix (Just m)) = set m ix

main :: IO ()
main = startApp

