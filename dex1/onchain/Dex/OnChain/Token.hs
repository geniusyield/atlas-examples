{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}
module Dex.OnChain.Token (
    mkTestTokenPolicy,
    MyTestDatum (..)
) where

import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Value    (flattenValue)
import qualified PlutusTx
import           PlutusTx.Prelude          as PlutusTx



data MyTestDatum = MyTestDatum
  { mtdVals        :: [Integer]
  }
PlutusTx.unstableMakeIsData ''MyTestDatum

{-# INLINABLE mkTestTokenPolicy #-}
mkTestTokenPolicy :: Integer -> TokenName -> TxOutRef -> BuiltinData -> BuiltinData -> ()
mkTestTokenPolicy amt tn utxo _ ctx'
    | hasn'tUTxO  = traceError "UTxO not consumed"
    | tn /= tn'   = traceError "wrong token"
    | amt /= amt' = traceError "wrong amount"
    | otherwise   = ()
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    [(_, tn', amt')] = flattenValue $ txInfoMint info

    hasn'tUTxO :: Bool
    hasn'tUTxO = all (\i -> txInInfoOutRef i /= utxo) $ txInfoInputs info
