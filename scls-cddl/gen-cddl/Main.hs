{-# LANGUAGE LambdaCase #-}
module Main where

import Cardano.SCLS.CDDL (namespaces)

import qualified Codec.CBOR.Cuddle.Huddle as Cuddle
import Codec.CBOR.Cuddle.Pretty ()
import System.FilePath ((</>), (<.>))
import System.IO
import Prettyprinter.Render.Text (hPutDoc)
import Prettyprinter (pretty)
import System.Environment (getArgs)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map

main :: IO ()
main = getArgs >>= \case
  [dir] -> forM_ (Map.toList namespaces) $ \(ns, cddl) -> do
    writeSpec cddl (dir </> ns <.> "cddl")
  _ -> error "Usage: gen-cddl directory"

writeSpec :: Cuddle.Huddle -> FilePath -> IO ()
writeSpec hddl path =
  let cddl = Cuddle.toCDDLNoRoot hddl
      preface = "; This file was auto-generated from huddle. Please do not modify it directly!\n"
   in withFile path WriteMode $ \h -> do
        hPutStrLn h preface
        hPutDoc h (pretty cddl)
        -- Write an empty line at the end of the file
        hPutStrLn h ""
