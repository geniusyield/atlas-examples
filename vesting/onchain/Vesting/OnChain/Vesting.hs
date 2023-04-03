{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Vesting.OnChain.Vesting
    ( mkWrappedVestingValidator
    ) where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            UnsafeFromData (unsafeFromBuiltinData),
                                            from)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx.Prelude          (Bool, check, traceIfFalse, ($),
                                            (&&))

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkVestingValidator beneficiary deadline () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info beneficiary

    deadlineReached :: Bool
    deadlineReached = from deadline `contains` txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator pkh deadline _ ctx = check $ mkVestingValidator pkh (unsafeFromBuiltinData deadline) () (unsafeFromBuiltinData ctx)
