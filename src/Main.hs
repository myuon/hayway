module Main where

import qualified Lexer
import qualified SrcLoc
import qualified Outputable
import qualified HsSyn
import qualified Hayway.Parser
import qualified Hayway.Format

filename :: FilePath
filename = "./src/Main.hs"

loadAST :: FilePath -> IO (Either Outputable.SDoc (SrcLoc.Located (HsSyn.HsModule HsSyn.GhcPs)))
loadAST path = do
  r <- Hayway.Parser.parseModuleFile filename
  return $ case r of
    Lexer.POk _ h -> Right h
    Lexer.PFailed _ _ d -> Left d

recoverSrcLoc = undefined

main :: IO ()
main = do
  flags <- Hayway.Parser.getDynFlags

  loadAST filename >>= \case
    Left d -> print $ Outputable.showSDoc flags d
    Right r -> putStrLn $ Outputable.showSDoc flags $ Outputable.ppr $ SrcLoc.unLoc r
