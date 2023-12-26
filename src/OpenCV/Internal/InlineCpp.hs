{-# LANGUAGE TemplateHaskellQuotes #-}

module OpenCV.Internal.InlineCpp
    ( cvCtx
    , C'Mat
    , OpenCVException (..)
    ) where

import Control.Exception
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as CC
import Language.C.Inline.Cpp.Exception qualified as CE

cvCtx :: C.Context
cvCtx = CC.cppCtx <> C.bsCtx <> C.vecCtx <> cvCppTypesTableCtx

data C'Mat

cvCppTypesTableCtx :: C.Context
cvCppTypesTableCtx =
    CC.cppTypePairs
        [ ("cv::Mat", [t|C'Mat|])
        ]

data OpenCVException
    = OpenCVCppException !CE.CppException
    | OpenCVNullPtrException
    | OpenCVUnexpectedStatusException String
    deriving stock (Show)
    deriving anyclass (Exception)
