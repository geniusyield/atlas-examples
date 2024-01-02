{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Vesting.OnChain.Vesting
    ( mkWrappedVestingValidator
    ) where

import           PlutusLedgerApi.V1.Interval (contains, from)
import           PlutusLedgerApi.V2          (POSIXTime, PubKeyHash)
import           PlutusLedgerApi.V2.Contexts (ScriptContext (scriptContextTxInfo),
                                              TxInfo (txInfoValidRange),
                                              txSignedBy)
import           PlutusTx                    (BuiltinData,
                                              UnsafeFromData (unsafeFromBuiltinData))
import           PlutusTx.Prelude            (Bool, check, traceIfFalse, ($),
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
