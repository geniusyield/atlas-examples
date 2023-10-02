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
import Plutus.V1.Ledger.Value (assetClassValue, Value)
import GeniusYield.TxBuilder (valueFromPlutus')

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
    cprUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data ClosePoolParams = ClosePoolParams
  { clppGPParams  :: !GeneralParams
  , clppFactoryAssetClass  :: !GYAssetClass
  , clppTokenAAssetClass :: !GYAssetClass
  , clppTokenBAssetClass :: !GYAssetClass
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data ClosePoolResponse = ClosePoolResponse
  { 
    clprUnsignedTxResponse :: !UnsignedTxResponse
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data ListPoolParams = ListPoolParams
  { lppFactoryAssetClass  :: !GYAssetClass
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data SinglePoolResponse = SinglePoolResponse {
  sprCoinA :: !GYValue,
  sprCoinB :: !GYValue
  } deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema)

data ListPoolResponse = ListPoolResponse
  { 
    lprList :: ![SinglePoolResponse]
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

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
    { rpCoinA :: !GYAssetClass           -- ^ One 'Coin' of the liquidity pair.
    , rpCoinB :: !GYAssetClass           -- ^ The other 'Coin' of the liquidity pair.
    , rpDiff  :: !Integer -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
    { apCoinA   :: GYAssetClass         -- ^ One 'Coin' of the liquidity pair.
    , apCoinB   :: GYAssetClass         -- ^ The other 'Coin' of the liquidity pair.
    , apAmountA :: !Integer       -- ^ The amount of coins of the first kind to add to the pool.
    , apAmountB :: !Integer       -- ^ The amount of coins of the second kind to add to the pool.
    } deriving (Show, Generic, ToJSON, FromJSON)

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
  :<|> "pool" :> "close"
    :> ReqBody '[JSON] ClosePoolParams
    :> Post    '[JSON] ClosePoolResponse
  :<|> "pool" :> "list"
    :> ReqBody '[JSON] ListPoolParams
    :> Post    '[JSON] ListPoolResponse
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
                :<|> handleClosePool ctx
                :<|> handleListPool ctx
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
      --tokenA = valueSingleton cppTokenAAssetClass cppTokenAAmount
      --tokenB = valueSingleton cppTokenBAssetClass cppTokenBAmount
  -- fail $ printf "tokenA %s" tokenA
  txBody <- runTxI ctx (gpUsedAddrs cppGPParams) (gpChangeAddr cppGPParams) (gpCollateral cppGPParams)
              $ createPool 
                (gpChangeAddr cppGPParams)
                  us 
                  (Script'.Coin $ assetClassToPlutus cppTokenAAssetClass)
                  (Script'.Amount cppTokenAAmount) 
                  (Script'.Coin $ assetClassToPlutus cppTokenBAssetClass)
                  (Script'.Amount cppTokenBAmount)  
  pure $ CreatePoolResponse (unSignedTxWithFee txBody Nothing)

handleClosePool :: Ctx -> ClosePoolParams -> IO ClosePoolResponse
handleClosePool ctx ClosePoolParams{..} = do
  txBody <- runTxI ctx (gpUsedAddrs clppGPParams) (gpChangeAddr clppGPParams) (gpCollateral clppGPParams)
              $ closePool 
                  (uniswap clppFactoryAssetClass)
                  (Script'.Coin $ assetClassToPlutus clppTokenAAssetClass)
                  (Script'.Coin $ assetClassToPlutus clppTokenBAssetClass)
  pure $ ClosePoolResponse (unSignedTxWithFee txBody Nothing)


handleListPool :: Ctx -> ListPoolParams -> IO ListPoolResponse
handleListPool ctx ListPoolParams{..} = do
  let us   = uniswap lppFactoryAssetClass
  poolsList <- runQuery ctx $ poolsGY us 

  pure $ ListPoolResponse (go poolsList)
    where 
      go :: [(GYValue, GYValue)] -> [SinglePoolResponse]
      go [] = []
      go ((aV, bV) : xs) = do
        let p' = SinglePoolResponse aV bV
        p' : go xs



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
