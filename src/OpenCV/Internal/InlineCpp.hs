{-# LANGUAGE TemplateHaskellQuotes #-}

module OpenCV.Internal.InlineCpp where

import Data.Map qualified as M
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as CC
import Language.C.Types qualified as C

cvCtx :: C.Context
cvCtx = CC.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty{C.ctxTypesTable = cvTypesTable}

data C'Mat

cvTypesTable :: C.TypesTable
cvTypesTable =
    M.fromList
        [ (C.TypeName "Mat", [t|C'Mat|])
        ]
