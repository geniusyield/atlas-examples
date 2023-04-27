module Dex.Api.Dex where

import           Dex.Api.Context
import           Dex.Api.Operations
import qualified Dex.OnChain.Dex.Compiled as Script
import qualified Dex.OnChain.Uniswap.Types as Script'
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
  , ctpDatumCount :: !Integer
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Return type for our API endpoints defined here.
data UnsignedTxResponse = UnsignedTxResponse
  { urspTxBodyHex  :: !T.Text           -- ^ Unsigned transaction cbor.
  , urspTxFee      :: !(Maybe Integer)  -- ^ Tx fees.
  , urspUtxoRefIdx :: !(Maybe Word)     -- ^ Some operations might need to show for relevant UTxO generated, this index will let UI know of it. Note that Transaction ID would change after getting signed.
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data HelloWorldParams = HelloWorldParams
  { hwpUsedAddrs  :: ![GYAddress]
  , hwpAddr :: !GYAddress
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data GeneralParams = GeneralParams
  { gpUsedAddrs  :: ![GYAddress]
  , gpChangeAddr :: !GYAddress
  , gpCollateral :: !GYTxOutRefCbor
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data StartParams = StartParams
  { spGPParams  :: !GeneralParams
  , spTokenName :: !GYTokenName
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data StartFactoryParams = StartFactoryParams
  { spfGPParams  :: !GeneralParams
  , spfAssetClass :: !GYAssetClass
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data StartResponse = StartResponse
  { srAssetClass  :: !GYAssetClass
  , srUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data StartFactoryResponse = StartFactoryResponse
  { srfUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data HelloWorldResponse = HelloWorldResponse
  { hwrMessage :: ![T.Text]
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data HelloWorldDataResponse = HelloWorldDataResponse
  { hwdrMessage :: ![(GYTxOutRef, [Integer])]
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

-- | Construct `UnsignedTxResponse` return type for our endpoint given the transaction body & relevant index for UTxO (if such exists).
unSignedTxWithFee :: GYTxBody -> Maybe Word -> UnsignedTxResponse
unSignedTxWithFee txBody mUtxoRefIdx = UnsignedTxResponse
  { urspTxBodyHex  = T.pack $ txToHex $ unsignedTx txBody
  , urspTxFee      = Just $ txBodyFee txBody
  , urspUtxoRefIdx = mUtxoRefIdx
  }

type DexApi =
       "start"
    :> ReqBody '[JSON] StartParams
    :> Post    '[JSON] StartResponse
  :<|> "factory"
    :> ReqBody '[JSON] StartFactoryParams
    :> Post    '[JSON] StartFactoryResponse
  :<|> "token"
    :> ReqBody '[JSON] CreateTokensParams
    :> Post    '[JSON] UnsignedTxResponse
  :<|> "hello"
    :> ReqBody '[JSON] HelloWorldParams
    :> Post    '[JSON] HelloWorldDataResponse

handleDexApi :: Ctx -> ServerT DexApi IO
handleDexApi ctx =   handleStart ctx
                :<|> handleFactory ctx
                :<|> handleCreateDatumToken ctx
                :<|> handleHello ctx

handleStart :: Ctx -> StartParams -> IO StartResponse
handleStart ctx StartParams{..} = do
  (ac, txBody) <- runTxI'' ctx (gpUsedAddrs spGPParams) (gpChangeAddr spGPParams) (gpCollateral spGPParams)
              $ mintTestTokens spTokenName 1
  pure $ StartResponse ac (unSignedTxWithFee txBody Nothing)


uniswap :: GYAssetClass -> Script'.Uniswap
uniswap ac = Script'.Uniswap $ Script.mkCoin' (assetClassToPlutus ac) 

handleFactory :: Ctx -> StartFactoryParams -> IO StartFactoryResponse
handleFactory ctx StartFactoryParams{..} = do
  let c    = Script.mkCoin' $ assetClassToPlutus spfAssetClass
      us   = uniswap spfAssetClass
      -- inst = uniswapInstance us  
  txBody <- runTxI ctx (gpUsedAddrs spfGPParams) (gpChangeAddr spfGPParams) (gpCollateral spfGPParams)
              $ createFactory us c
  pure $ StartFactoryResponse (unSignedTxWithFee txBody Nothing) 


-- | Handle for place bet operation.
handleCreateToken :: Ctx -> CreateTokensParams -> IO UnsignedTxResponse
handleCreateToken ctx CreateTokensParams{..} = do
  txBody <- runTxI ctx ctpUsedAddrs ctpChangeAddr ctpCollateral
              $ mintTestTokens' ctpName 1
  pure $ unSignedTxWithFee txBody Nothing
-- | Handle for place bet operation.
handleCreateDatumToken :: Ctx -> CreateTokensParams -> IO UnsignedTxResponse
handleCreateDatumToken ctx CreateTokensParams{..} = do
  txBody <- runTxI ctx ctpUsedAddrs ctpChangeAddr ctpCollateral
              $ mintDatumTokens ctpChangeAddr ctpName 1 ctpDatumCount
  pure $ unSignedTxWithFee txBody Nothing

handleHello :: Ctx -> HelloWorldParams -> IO HelloWorldDataResponse
handleHello ctx HelloWorldParams{..} = do
  hwdr <- runQuery ctx $ listsDatum hwpUsedAddrs
  pure $ HelloWorldDataResponse hwdr
  {-
  return HelloWorldDataResponse { 
    hwdrMessage = [(ref, Script.mtdVals dat) | (ref, dat) <- hwdr]
  }
  -}
