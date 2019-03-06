module Hayway.Parser(
  getDynFlags,
  runParser,
  parseModuleFile
) where

import qualified Lexer
import qualified Parser
import qualified StringBuffer
import qualified SrcLoc
import qualified FastString
import qualified Outputable
import qualified GHC
import qualified GHC.Paths

getDynFlags :: IO (GHC.DynFlags)
getDynFlags = GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags

runParser :: GHC.DynFlags -> FilePath -> Lexer.P a -> IO (Lexer.ParseResult a)
runParser flags path parser = do
  file <- StringBuffer.hGetStringBuffer path

  let pstate = Lexer.mkPState flags file (SrcLoc.mkRealSrcLoc (FastString.fsLit "") 0 0)
  return $ Lexer.unP parser pstate

parseModuleFile :: FilePath -> IO (Lexer.ParseResult (GHC.Located (GHC.HsModule GHC.GhcPs)))
parseModuleFile path = do
  flags <- getDynFlags
  runParser flags path Parser.parseModule
