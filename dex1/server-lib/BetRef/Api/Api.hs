module BetRef.Api.Api where

import           BetRef.Api.BetRef   (BetRefApi, handleBetRefApi)
import           BetRef.Api.Context
import           BetRef.Api.Tx       (TxAPI, handleTx)
import           Data.Swagger
import           GeniusYield.Imports
import           Servant
import           Servant.Swagger

-- | Type for our Servant API.
type Api =
        "tx"  :> TxAPI
  :<|>  "betref" :>  BetRefApi

appApi :: Proxy Api
appApi = Proxy

apiSwagger  :: Swagger
apiSwagger  = toSwagger appApi

apiServer :: Ctx -> ServerT Api IO
apiServer ctx =
       handleTx ctx
  :<|> handleBetRefApi ctx
