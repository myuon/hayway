{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lexer
import qualified Outputable
import qualified Hayway.Parser

filename :: FilePath
filename = "./src/Main.hs"

main :: IO ()
main = do
  flags <- Hayway.Parser.getDynFlags
  r <- Hayway.Parser.parseModuleFile filename
  case r of
    Lexer.POk _ h -> putStrLn $ Outputable.showSDoc flags $ Outputable.ppr h
    _ -> error "failed"
