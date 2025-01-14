import Control.Monad (unless)
import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Environment (getArgs)
import Text.Printf (printf)
import Vesting.Api

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile] -> return (coreCfgFile, skeyFile)
        _invalidArgument -> fail "Error: wrong arguments, needed the configuration file and the beneficiary skey file\n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile) <- parseArgs
    printf "configuration file: %s\nbeneficiary skey file: %s\n" coreCfgFile skeyFile
    coreCfg <- coreConfigIO coreCfgFile
    skey <- readSigningKey @'GYKeyRolePayment skeyFile
    let nid = cfgNetworkId coreCfg
        beneficiaryPkh = verificationKeyHash $ getVerificationKey skey
        beneficiaryPkh' = toPubKeyHash beneficiaryPkh
        beneficiaryAddr = addressFromPaymentKeyHash nid beneficiaryPkh
    withCfgProviders coreCfg "retrieve-vesting" $ \providers -> do
        addr <- runGYTxQueryMonadIO nid providers $ vestingAddress beneficiaryPkh'
        printf "vesting address: %s\n" $ addressToBech32 addr

        vs <- runGYTxQueryMonadIO nid providers $ availableVestings beneficiaryPkh'
        printf "found %d available vesting(s)\n" $ length vs
        unless (null vs) $ do
            txBody <- runGYTxBuilderMonadIO nid providers [beneficiaryAddr] beneficiaryAddr Nothing $ retrieveVestings beneficiaryPkh' vs >>= buildTxBody
            tid <- gySubmitTx providers $ signGYTxBody txBody [skey]
            printf "submitted tx: %s\n" tid
