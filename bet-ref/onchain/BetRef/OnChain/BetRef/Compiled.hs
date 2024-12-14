-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module BetRef.OnChain.BetRef.Compiled (
  betRefValidator,
  BetRefParams (..),
  OracleAnswerDatum (..),
  BetRefDatum (..),
  BetRefAction (..),
) where

import PlutusCore.Version (plcVersion110)
import PlutusTx qualified

import BetRef.OnChain.BetRef

-- Since makeLift doesn't seem to work on BetRefParams. We just convert it to data and apply that instead.
betRefValidator :: BetRefParams -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> ())
betRefValidator betRefParams =
  $$(PlutusTx.compile [||mkBetRefValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 (PlutusTx.toBuiltinData betRefParams)
