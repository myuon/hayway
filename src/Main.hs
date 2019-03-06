{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lexer
import qualified Parser
import qualified StringBuffer
import qualified SrcLoc
import qualified FastString
import qualified Outputable
import qualified GHC
import qualified GHC.Paths
import qualified GHCParser

filename :: FilePath
filename = "./src/Main.hs"

main :: IO ()
main = do
  flags <- GHCParser.getDynFlags
  r <- GHCParser.parseModuleFile filename
  case r of
    Lexer.POk _ h -> putStrLn $ Outputable.showSDoc flags $ Outputable.ppr h
    _ -> error "failed"
