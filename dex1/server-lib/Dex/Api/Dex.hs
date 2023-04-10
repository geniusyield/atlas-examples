module Dex.Api.Dex where

import           Dex.Api.Context
import           Dex.Api.Operations
import qualified Dex.OnChain.Dex.Compiled as Script
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromJust)
import qualified Data.Swagger                   as Swagger
import qualified Data.Text                      as T
import qualified GeniusYield.Examples.Limbo     as Limbo
import           GeniusYield.Imports
import           GeniusYield.Types
import           Servant

data TokenParams = TokenParams
  { tpName        :: !GYTokenName
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data CreateTokensParams = CreateTokensParams
  { ctpUsedAddrs  :: ![GYAddress]
  , ctpChangeAddr :: !GYAddress
  , ctpCollateral :: !GYTxOutRefCbor
  , ctpName :: !GYTokenName
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Return type for our API endpoints defined here.
data UnsignedTxResponse = UnsignedTxResponse
  { urspTxBodyHex  :: !T.Text           -- ^ Unsigned transaction cbor.
  , urspTxFee      :: !(Maybe Integer)  -- ^ Tx fees.
  , urspUtxoRefIdx :: !(Maybe Word)     -- ^ Some operations might need to show for relevant UTxO generated, this index will let UI know of it. Note that Transaction ID would change after getting signed.
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

-- | Construct `UnsignedTxResponse` return type for our endpoint given the transaction body & relevant index for UTxO (if such exists).
unSignedTxWithFee :: GYTxBody -> Maybe Word -> UnsignedTxResponse
unSignedTxWithFee txBody mUtxoRefIdx = UnsignedTxResponse
  { urspTxBodyHex  = T.pack $ txToHex $ unsignedTx txBody
  , urspTxFee      = Just $ txBodyFee txBody
  , urspUtxoRefIdx = mUtxoRefIdx
  }

type DexApi =
       "token"
    :> ReqBody '[JSON] CreateTokensParams
    :> Post    '[JSON] UnsignedTxResponse

handleDexApi :: Ctx -> ServerT DexApi IO
handleDexApi ctx =   handleCreateToken ctx

-- | Handle for place bet operation.
handleCreateToken :: Ctx -> CreateTokensParams -> IO UnsignedTxResponse
handleCreateToken ctx CreateTokensParams{..} = do
  txBody <- runTxI ctx ctpUsedAddrs ctpChangeAddr ctpCollateral
              $ mintTestTokens' ctpName 1
  pure $ unSignedTxWithFee txBody Nothing
