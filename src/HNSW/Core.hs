{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HNSW.Core where

import Control.Exception
import Foreign.ForeignPtr
    ( ForeignPtr
    , newForeignPtr
    , withForeignPtr
    )
import Foreign.Ptr (Ptr, nullPtr)
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as CE
import OpenCV.Internal.InlineCpp -- todo

C.context cvCtx

C.include "hnswlib/hnswlib.h"

newtype HierarchicalNSW = HierarchicalNSW {unHierarchicalNSW :: ForeignPtr (C'HierarchicalNSW C.CFloat)}

fromIOPtr :: IO (Ptr (C'HierarchicalNSW C.CFloat)) -> IO HierarchicalNSW
fromIOPtr ioPtr = mask_ $ do
    ptr <- ioPtr
    let deletePtr = [C.funPtr|void deleteHnsw(hnswlib::HierarchicalNSW<float>* nsw) { delete nsw; }|]
    HierarchicalNSW <$> newForeignPtr deletePtr ptr

algHnswNew :: Int -> Int -> Int -> Int -> IO HierarchicalNSW
algHnswNew dim m efConstruction maxElements =
    fromIOPtr
        [C.block|hnswlib::HierarchicalNSW<float>*{
      new
    }|]
