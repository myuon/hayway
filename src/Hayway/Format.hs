module Hayway.Format (formatModule) where

import qualified SrcLoc
import qualified HsSyn
import qualified HsDecls
import qualified Module
import qualified Data.Heap as Heap
import Control.Monad.State.CPS
import qualified Data.Foldable as F

data Layout = Layout Int Int

incrementOrder :: State Layout ()
incrementOrder = modify (\(Layout x y) -> Layout (x+1) y)

incrementIndent :: State Layout ()
incrementIndent = modify (\(Layout x y) -> Layout x (y+1))

type LocHeap = Heap.Heap (Heap.Entry SrcLoc.SrcLoc Layout)

getHeadLoc :: SrcLoc.Located a -> SrcLoc.SrcLoc
getHeadLoc = SrcLoc.srcSpanStart . SrcLoc.getLoc

formatModule :: SrcLoc.SrcLoc -> HsSyn.HsModule pass -> State Layout LocHeap
formatModule srcloc hsmod = F.foldlM (\heap -> fmap (Heap.union heap)) Heap.empty [locModDecl, locDecls]
  where
    locDecls =
      F.foldlM (\heap ldecl ->
        fmap (Heap.union heap) $
        formatDecl (getHeadLoc ldecl) (SrcLoc.unLoc ldecl)
      ) Heap.empty (HsSyn.hsmodDecls hsmod)
    
    locModDecl = maybe (return Heap.empty) (\name -> do
        layout <- get
        incrementOrder
        return $ Heap.singleton $ Heap.Entry (getHeadLoc name) layout
      ) (HsSyn.hsmodName hsmod)

formatDecl :: SrcLoc.SrcLoc -> HsDecls.HsDecl pass -> State Layout LocHeap
formatDecl = undefined
