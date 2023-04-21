import           Control.Monad         (unless)
import           GeniusYield.GYConfig
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           System.Environment    (getArgs)
import           Text.Printf           (printf)
import           Vesting.Api

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile] -> return (coreCfgFile, skeyFile)
        _invalidArgument        -> fail "Error: wrong arguments, needed the configuration file and the beneficiary skey file\n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile) <- parseArgs
    printf "configuration file: %s\nbeneficiary skey file: %s\n" coreCfgFile skeyFile
    coreCfg <- coreConfigIO coreCfgFile
    skey    <- readPaymentSigningKey skeyFile
    let nid             = cfgNetworkId coreCfg
        beneficiaryPkh  = pubKeyHash $ paymentVerificationKey skey
        beneficiaryAddr = addressFromPubKeyHash nid beneficiaryPkh
    withCfgProviders coreCfg "retrieve-vesting" $ \providers -> do
        addr <- runGYTxQueryMonadNode nid providers $ vestingAddress beneficiaryPkh
        printf "vesting address: %s\n" $ addressToBech32 addr

        vs <- runGYTxQueryMonadNode nid providers $ availableVestings beneficiaryPkh
        printf "found %d available vesting(s)\n" $ length vs
        unless (null vs) $ do
            txBody <- runGYTxMonadNode nid providers [beneficiaryAddr] beneficiaryAddr Nothing $ retrieveVestings beneficiaryPkh vs
            tid     <- gySubmitTx providers $ signTx txBody [skey]
            printf "submitted tx: %s\n" tid
