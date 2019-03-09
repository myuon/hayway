module Main where

import qualified Lexer
import qualified SrcLoc
import qualified Outputable
import qualified HsSyn
import qualified Hayway.Parser
import qualified Hayway.Format
import qualified Data.Heap as Heap
import qualified Data.Foldable as F

filename :: FilePath
filename = "./src/Main.hs"

loadAST :: FilePath -> IO (Either Outputable.SDoc (SrcLoc.Located (HsSyn.HsModule HsSyn.GhcPs)))
loadAST path = do
  r <- Hayway.Parser.parseModuleFile filename
  return $ case r of
    Lexer.POk _ h -> Right h
    Lexer.PFailed _ _ d -> Left d

recoverSrcLoc :: [Heap.Entry SrcLoc.SrcLoc Hayway.Format.Layout] -> Outputable.SDoc
recoverSrcLoc = undefined

main :: IO ()
main = do
  flags <- Hayway.Parser.getDynFlags

  parsedResult <- loadAST filename
  
  case parsedResult of
    Left d -> putStrLn $ Outputable.showSDoc flags d
    Right r -> putStrLn $ Outputable.showSDoc flags $ recoverSrcLoc $ F.toList $ Hayway.Format.format $ SrcLoc.unLoc r
