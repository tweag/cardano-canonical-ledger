module Main (main) where

import qualified Cardano.SCLS.Internal.Version

main :: IO ()
main = const (putStrLn "Test suite not yet implemented.") (Cardano.SCLS.Internal.Version.packVersion)

