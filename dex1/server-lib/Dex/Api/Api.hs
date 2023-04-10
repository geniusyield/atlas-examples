module Dex.Api.Api where

import           Dex.Api.Dex   (DexApi, handleDexApi)
import           Dex.Api.Context
import           Dex.Api.Tx       (TxAPI, handleTx)
import           Data.Swagger
import           GeniusYield.Imports
import           Servant
import           Servant.Swagger

-- | Type for our Servant API.
type Api =
        "tx"  :> TxAPI
  :<|>  "dex" :>  DexApi

appApi :: Proxy Api
appApi = Proxy

apiSwagger  :: Swagger
apiSwagger  = toSwagger appApi

apiServer :: Ctx -> ServerT Api IO
apiServer ctx =
       handleTx ctx
  :<|> handleDexApi ctx
