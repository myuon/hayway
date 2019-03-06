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

filename :: FilePath
filename = "./src/Main.hs"

runParser :: GHC.DynFlags -> FilePath -> Lexer.P a -> IO (Lexer.ParseResult a)
runParser flags path parser = do
  file <- StringBuffer.hGetStringBuffer path

  let pstate = Lexer.mkPState flags file (SrcLoc.mkRealSrcLoc (FastString.fsLit "") 0 0)
  return $ Lexer.unP parser pstate

main :: IO ()
main = do
  flags <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  r <- runParser flags filename Parser.parseModule
  case r of
    Lexer.POk _ h -> putStrLn $ Outputable.showSDoc flags $ Outputable.ppr h
    _ -> error "failed"
