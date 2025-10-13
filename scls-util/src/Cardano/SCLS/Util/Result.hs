{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Util.Result (
  Result (..),
  toErrorCode,
) where

import GHC.IO.Exception (ExitCode (..))

data Result
  = Ok
  | VerifyFailure -- TODO: add more detailed errors and mapping to exit codes
  | OtherError

toErrorCode :: Result -> ExitCode
toErrorCode = \case
  Ok -> ExitSuccess
  VerifyFailure -> ExitFailure 65
  OtherError -> ExitFailure 1
