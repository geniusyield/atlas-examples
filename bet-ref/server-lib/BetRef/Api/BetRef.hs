module BetRef.Api.BetRef where

import           BetRef.Api.Context
import           BetRef.Api.Operations
import qualified BetRef.OnChain.BetRef.Compiled as Script
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromJust)
import qualified Data.Swagger                   as Swagger
import qualified Data.Text                      as T
import qualified GeniusYield.Examples.Limbo     as Limbo
import           GeniusYield.Imports
import           GeniusYield.Types
import           Servant

-- | Input wrapper around corresponding Plutus type.
data BetRefParams = BetRefParams
  { brpOracleAddress :: !GYAddress
  , brpBetUntil      :: !GYTime
  , brpBetReveal     :: !GYTime
  , brpBetStep       :: !GYValue
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Convert the above `BetRefParams` with corresponding representation defined in our Plutus validator script.
betParamsToScript :: BetRefParams -> Script.BetRefParams
betParamsToScript brp = Script.BetRefParams
  { Script.brpOraclePkh = pubKeyHashToPlutus $ fromJust $ addressToPubKeyHash $ brpOracleAddress brp
  , Script.brpBetUntil = timeToPlutus $  brpBetUntil brp
  , Script.brpBetReveal = timeToPlutus $ brpBetReveal brp
  , Script.brpBetStep = valueToPlutus $  brpBetStep brp
  }

-- | Input parameters for place bet operation.
data PlaceBetRefParams = PlaceBetRefParams
  { pbrUsedAddrs  :: ![GYAddress]
  , pbrChangeAddr :: !GYAddress
  , pbrCollateral :: !GYTxOutRefCbor
  , pbrBetParams  :: !BetRefParams
  , pbrBetGuess   :: !Integer
  , pbrBetAmt     :: !GYValue
  , pbrRefScript  :: !GYTxOutRef
  , pbrPrevBetRef :: !(Maybe GYTxOutRef)
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Input parameters for take bets operation.
data TakeBetRefParams = TakeBetRefParams
  { tbrUsedAddrs         :: ![GYAddress]
  , tbrChangeAddr        :: !GYAddress
  , tbrCollateral        :: !GYTxOutRefCbor
  , tbrBetParams         :: !BetRefParams
  , tbrRefScript         :: !GYTxOutRef
  , tbrPrevBetRef        :: !GYTxOutRef
  , tbrOracleRefInputRef :: !GYTxOutRef
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Input parameters to add for reference script.
data AddRefScriptParams = AddRefScriptParams
  { arsUsedAddrs  :: ![GYAddress]
  , arsChangeAddr :: !GYAddress
  , arsCollateral :: !GYTxOutRefCbor
  , arsPutAddress :: !GYAddress
  , arsBetParams  :: !BetRefParams
  } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Input parameters to add for reference input.
data AddRefInputParams = AddRefInputParams
  { ariUsedAddrs  :: ![GYAddress]
  , ariChangeAddr :: !GYAddress
  , ariCollateral :: !GYTxOutRefCbor
  , ariPutAddress :: !GYAddress
  , ariBetAnswer  :: !Integer
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

-- | Type for our Servant API.
type BetRefApi =
       "place"
    :> ReqBody '[JSON] PlaceBetRefParams
    :> Post    '[JSON] UnsignedTxResponse
  :<|> "take"
    :> ReqBody '[JSON] TakeBetRefParams
    :> Post    '[JSON] UnsignedTxResponse
  :<|> "add-ref-script"
    :> ReqBody  '[JSON] AddRefScriptParams
    :> Post     '[JSON] UnsignedTxResponse
  :<|> "add-ref-input"
    :> ReqBody  '[JSON] AddRefInputParams
    :> Post     '[JSON] UnsignedTxResponse

-- | Serving our API.
handleBetRefApi :: Ctx -> ServerT BetRefApi IO
handleBetRefApi ctx =   handlePlaceBet ctx
                   :<|> handleTakeBet ctx
                   :<|> handleAddRefScript ctx
                   :<|> handleOracleRefInput ctx

-- | Handle for place bet operation.
handlePlaceBet :: Ctx -> PlaceBetRefParams -> IO UnsignedTxResponse
handlePlaceBet ctx PlaceBetRefParams{..} = do
  let brp = betParamsToScript pbrBetParams
  validatorAddress <- runQuery ctx (betRefAddress brp)
  txBody <- runTxI ctx pbrUsedAddrs pbrChangeAddr pbrCollateral
              $ placeBet pbrRefScript (betParamsToScript pbrBetParams) (Script.OracleAnswerDatum pbrBetGuess) pbrBetAmt (head pbrUsedAddrs) pbrPrevBetRef
  placeUtxoRef <- case find (\utxo -> utxoAddress utxo == validatorAddress) $ utxosToList $ txBodyUTxOs txBody of
        Nothing -> fail "Shouldn't happen: No reference for placed bet in body"
        Just utxo -> pure $ utxoRef utxo
  pure $ unSignedTxWithFee txBody (Just $ snd $ txOutRefToTuple placeUtxoRef)

-- | Handle for take bets operation.
handleTakeBet :: Ctx -> TakeBetRefParams -> IO UnsignedTxResponse
handleTakeBet ctx TakeBetRefParams{..} = do
  txBody <- runTxI ctx tbrUsedAddrs tbrChangeAddr tbrCollateral
              $ takeBets tbrRefScript (betParamsToScript tbrBetParams) tbrPrevBetRef (head tbrUsedAddrs) tbrOracleRefInputRef
  pure $ unSignedTxWithFee txBody Nothing

-- | Handle for adding reference script.
handleAddRefScript :: Ctx -> AddRefScriptParams  -> IO UnsignedTxResponse
handleAddRefScript  ctx AddRefScriptParams{..} = do
  let validator = betRefValidator' (betParamsToScript arsBetParams)
  txBody <- runTxI ctx arsUsedAddrs arsChangeAddr arsCollateral
              $ pure $ addRefScript' arsPutAddress validator
  let refs   = Limbo.findRefScriptsInBody txBody
  outRef <- case Map.lookup (Some (validatorToScript validator)) refs of
                 Nothing  -> fail "Shouldn't happen: No reference for added Script in body"
                 Just ref -> return ref
  pure $ unSignedTxWithFee txBody (Just $ snd $ txOutRefToTuple outRef)

-- | Handle for adding reference input.
handleOracleRefInput :: Ctx -> AddRefInputParams  -> IO UnsignedTxResponse
handleOracleRefInput  ctx AddRefInputParams{..} = do
  let ourDatumPlutus = Script.OracleAnswerDatum ariBetAnswer
      ourDatumGY = datumFromPlutusData ourDatumPlutus
  txBody <- runTxI ctx ariUsedAddrs ariChangeAddr ariCollateral
              $ pure $ addRefInput' ariPutAddress ourDatumPlutus
  let utxos = utxosToList $ txBodyUTxOs txBody
      ourDatumHash = hashDatum ourDatumGY
      mRefInputUtxo = find (\utxo ->
        case utxoOutDatum utxo of
          GYOutDatumHash dh  -> ourDatumHash == dh
          GYOutDatumInline d -> ourDatumGY == d
          GYOutDatumNone     -> False
        ) utxos
  case mRefInputUtxo of
    Nothing               -> fail "Shouldn't happen: Couldn't find the desired UTxO in Tx outputs"
    Just GYUTxO {utxoRef} -> pure $ unSignedTxWithFee txBody (Just $ snd $ txOutRefToTuple utxoRef)
