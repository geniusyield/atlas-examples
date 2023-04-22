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

-- | Input parameters to add for reference script.
data AddWitAndSubmitParams = AddWitAndSubmitParams
  { awasTxUnsigned :: !GYTx
  , awasTxWit      :: !GYTxWitness
  } deriving (Generic, FromJSON, Swagger.ToSchema)

-- | Construct `SubmitTxResponse` return type from the given signed transaction body.
txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody = SubmitTxResponse
                                  { submitTxFee = txBodyFee txBody
                                  , submitTxId  = txBodyTxId txBody
                                  }

-- | Type for our Servant API.
type TxAPI =
      "add-wit-and-submit"
    :> ReqBody '[JSON] AddWitAndSubmitParams
    :> Post '[JSON] SubmitTxResponse

-- | Serving our API.
handleTx :: Ctx -> ServerT TxAPI IO
handleTx = handleAddWitAndSubmitTx

-- | Handle for adding key witness to the unsigned transaction & then submit it.
handleAddWitAndSubmitTx :: Ctx -> AddWitAndSubmitParams -> IO SubmitTxResponse
handleAddWitAndSubmitTx ctx AddWitAndSubmitParams{..} = do
  let txBody = getTxBody awasTxUnsigned
  void $ gySubmitTx (ctxProviders ctx) $ makeSignedTransaction awasTxWit txBody
  return $ txBodySubmitTxResponse txBody
