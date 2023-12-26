{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Mat where

import Control.Exception (mask_)
import Foreign.ForeignPtr
    ( ForeignPtr
    , newForeignPtr
    , withForeignPtr
    )
import Foreign.Ptr (Ptr, nullPtr)
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as C
import OpenCV.Internal.InlineCpp (C'Mat, cvCtx)

C.context cvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgcodecs.hpp"
C.include "<iostream>"
C.using "namespace cv"

newtype Mat = Mat {unMat :: ForeignPtr C'Mat}

newEmptyMat :: IO Mat
newEmptyMat = fromIOPtr [C.exp|Mat*{ new Mat() }|]

fromIOPtr :: IO (Ptr C'Mat) -> IO Mat
fromIOPtr ioPtr = mask_ $ do
    ptr <- ioPtr
    unsafeFromPtr ptr

unsafeFromPtr :: Ptr C'Mat -> IO Mat
unsafeFromPtr ptr = do
    let deleteMat = [C.funPtr| void deleteMat(Mat * m) { delete m; } |]
    Mat <$> newForeignPtr deleteMat ptr

fromPtrMaybe :: IO (Ptr C'Mat) -> IO (Maybe Mat)
fromPtrMaybe ioPtr = mask_ $ do
    ptr <- ioPtr
    if ptr == nullPtr
        then return Nothing
        else Just <$> unsafeFromPtr ptr

withMatPtr :: Mat -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat

printMat :: Mat -> IO ()
printMat mat = do
    withMatPtr mat $ \matPtr ->
        [C.block|void {
          std::cout << *$(Mat * matPtr) << std::endl;
        }|]
