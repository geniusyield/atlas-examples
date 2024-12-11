import Data.Text (pack)
import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Vesting.Api

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath, GYPubKeyHash, GYTime, Natural)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile, beneficiary, deadline', lovelace'] -> do
            let mpkh = do
                    addr <- addressFromTextMaybe $ pack beneficiary
                    addressToPubKeyHash addr
                mdeadline = gyIso8601ParseM deadline'
                mlovelace = readMaybe lovelace'
            case (mpkh, mdeadline, mlovelace) of
                (Just pkh, Just deadline, Just lovelace) -> return (coreCfgFile, skeyFile, pkh, deadline, lovelace)
                (Nothing, _, _) -> fail "Error: beneficiary address invalid\n"
                (_, Nothing, _) -> fail "Error: deadline invalid\n"
                (_, _, Nothing) -> fail "Error: invalid lovelace amount\n"
        _invalidArgument ->
            fail
                "Error: wrong arguments, needed the configuration file, the sender skey file, the beneficiary address, the deadline and the amount\n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile, beneficiary, deadline, lovelace) <- parseArgs
    printf "configuration file: %s\nsender skey file: %s\nbeneficiary: %s\ndeadline: %s\namount: %d\n" coreCfgFile skeyFile beneficiary deadline lovelace
    coreCfg <- coreConfigIO coreCfgFile
    skey <- readPaymentSigningKey skeyFile
    let nid = cfgNetworkId coreCfg
        sender = addressFromPaymentKeyHash nid $ paymentKeyHash $ paymentVerificationKey skey
    withCfgProviders coreCfg "place-vesting" $ \providers -> do
        addr <- runGYTxQueryMonadIO nid providers $ vestingAddress beneficiary
        printf "vesting address: %s\n" $ addressToBech32 addr

        txBody <-
            runGYTxBuilderMonadIO nid providers [sender] sender Nothing $
                placeVesting
                    beneficiary
                    deadline
                    (valueFromLovelace $ toInteger lovelace)
                    >>= buildTxBody
        tid <- gySubmitTx providers $ signGYTxBody txBody [skey]
        printf "submitted tx: %s\n" tid
