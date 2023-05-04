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

data ListBalanceParams = ListBalanceParams
  { lbpUsedAddrs  :: ![GYAddress]
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data GeneralParams = GeneralParams
  { gpUsedAddrs  :: ![GYAddress]
  , gpChangeAddr :: !GYAddress
  , gpCollateral :: !GYTxOutRefCbor
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data StartParams = StartParams
  { spGPParams  :: !GeneralParams
  , spTokenName :: !GYTokenName
  , spAmount    :: !Natural
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data StartFactoryParams = StartFactoryParams
  { spfGPParams  :: !GeneralParams
  , spfAssetClass :: !GYAssetClass
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data ListFactoryParams = ListFactoryParams
  { lfAssetClass :: !GYAssetClass
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data StartResponse = StartResponse
  { srAssetClass  :: !GYAssetClass
  , srUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data CreatePoolParams = CreatePoolParams
  { cppGPParams  :: !GeneralParams
  , cppFactoryAssetClass  :: !GYAssetClass
  , cppTokenAAssetClass :: !GYAssetClass
  , cppTokenAAmount :: !Integer
  , cppTokenBAssetClass :: !GYAssetClass
  , cppTokenBAmount :: !Integer
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data CreatePoolResponse = CreatePoolResponse
  { 
    -- cprUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data StartFactoryResponse = StartFactoryResponse
  { srfUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data ListFactoryResponse = ListFactoryResponse
  { lfMessage :: ![(GYTxOutRef, String)]
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data HelloWorldResponse = HelloWorldResponse
  { hwrMessage :: ![T.Text]
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data ListBalanceResponse = ListBalanceResponse
  { lbrValue :: !GYValue
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
       "token" :> "create"
    :> ReqBody '[JSON] StartParams
    :> Post    '[JSON] StartResponse
  :<|> "factory" :> "create"
    :> ReqBody '[JSON] StartFactoryParams
    :> Post    '[JSON] StartFactoryResponse
  :<|> "factory" :> "list"
    :> ReqBody '[JSON] ListFactoryParams
    :> Post    '[JSON] ListFactoryResponse
  :<|> "pool" :> "create"
    :> ReqBody '[JSON] CreatePoolParams
    :> Post    '[JSON] CreatePoolResponse
  :<|> "wallet" :> "balance"
    :> ReqBody '[JSON] ListBalanceParams
    :> Post    '[JSON] ListBalanceResponse
  :<|> "token" :> "createDatumToken"
    :> ReqBody '[JSON] CreateTokensParams
    :> Post    '[JSON] UnsignedTxResponse
  :<|> "token" :> "listDatum"
    :> ReqBody '[JSON] HelloWorldParams
    :> Post    '[JSON] HelloWorldDataResponse

handleDexApi :: Ctx -> ServerT DexApi IO
handleDexApi ctx =   handleStart ctx
                :<|> handleFactory ctx
                :<|> handleListFactory ctx
                :<|> handleCreatePool ctx
                :<|> handleListBalance ctx
                :<|> handleCreateDatumToken ctx
                :<|> handleHello ctx

handleStart :: Ctx -> StartParams -> IO StartResponse
handleStart ctx StartParams{..} = do
  (ac, txBody) <- runTxI'' ctx (gpUsedAddrs spGPParams) (gpChangeAddr spGPParams) (gpCollateral spGPParams)
              $ mintTestTokens spTokenName spAmount
  pure $ StartResponse ac (unSignedTxWithFee txBody Nothing)


uniswap :: GYAssetClass -> Script'.Uniswap
uniswap ac = Script'.Uniswap $ Script.mkCoin' (assetClassToPlutus ac) 

handleFactory :: Ctx -> StartFactoryParams -> IO StartFactoryResponse
handleFactory ctx StartFactoryParams{..} = do
  let us   = uniswap spfAssetClass
      -- inst = uniswapInstance us  
  txBody <- runTxI ctx (gpUsedAddrs spfGPParams) (gpChangeAddr spfGPParams) (gpCollateral spfGPParams)
              $ createFactory us
  pure $ StartFactoryResponse (unSignedTxWithFee txBody Nothing) 

handleCreatePool :: Ctx -> CreatePoolParams -> IO CreatePoolResponse
handleCreatePool ctx CreatePoolParams{..} = do
  let us   = uniswap cppFactoryAssetClass
      tokenA = valueSingleton cppTokenAAssetClass cppTokenAAmount
      tokenB = valueSingleton cppTokenBAssetClass cppTokenBAmount
  -- fail $ printf "tokenA %s" tokenA
  txBody <- runTxI ctx (gpUsedAddrs cppGPParams) (gpChangeAddr cppGPParams) (gpCollateral cppGPParams)
              $ createPool us tokenA tokenB      
  pure $ CreatePoolResponse 

handleListFactory :: Ctx -> ListFactoryParams -> IO ListFactoryResponse
handleListFactory ctx ListFactoryParams{..} = do
  let us   = uniswap lfAssetClass
  lfr <- runQuery ctx $ listFactory us 
  pure $ ListFactoryResponse lfr


handleListBalance :: Ctx -> ListBalanceParams -> IO ListBalanceResponse
handleListBalance ctx ListBalanceParams{..} = do
  val <- runQuery ctx $ listBalance' lbpUsedAddrs
  pure $ ListBalanceResponse val

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
