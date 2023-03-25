module BetRef.Api.Tx (TxAPI, handleTx) where
import           Servant

import           GeniusYield.Imports
import           GeniusYield.Types

import           BetRef.Api.Context
import qualified Data.Swagger        as Swagger


-- | Return type of API when submitting a transaction.
data SubmitTxResponse = SubmitTxResponse
                          { submitTxFee :: !Integer
                          , submitTxId  :: !GYTxId
                          } deriving (Show, Generic, ToJSON, Swagger.ToSchema)

-- | Construct `SubmitTxResponse` return type from the given signed transaction body.
txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody = SubmitTxResponse
                                  { submitTxFee = txBodyFee txBody
                                  , submitTxId  = txBodyTxId txBody
                                  }

-- | Type for our Servant API.
type TxAPI =
  "submit" :> ReqBody '[JSON] GYTx :> Post '[JSON] SubmitTxResponse

-- | Serving our API.
handleTx :: Ctx -> ServerT TxAPI IO
handleTx = handleSubmitTx

-- | Handle for submit operation.
handleSubmitTx :: Ctx -> GYTx -> IO SubmitTxResponse
handleSubmitTx ctx tx = do
    void (gySubmitTx (ctxProviders ctx) tx)
    return $ txBodySubmitTxResponse (getTxBody tx)
