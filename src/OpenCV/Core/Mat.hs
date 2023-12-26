{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Mat where

import Control.Exception (mask_, throwIO)
import Control.Monad (when)
import Foreign.ForeignPtr
    ( ForeignPtr
    , newForeignPtr
    , withForeignPtr
    )
import Foreign.Ptr (Ptr, nullPtr)
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as CE
import OpenCV.Internal.InlineCpp

C.context cvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgcodecs.hpp"
C.include "<iostream>"

newtype Mat = Mat {unMat :: ForeignPtr C'Mat}

newEmptyMat :: IO Mat
newEmptyMat = fromIOPtr [C.exp|cv::Mat*{ new cv::Mat() }|]

fromIOPtr :: IO (Ptr C'Mat) -> IO Mat
fromIOPtr ioPtr = mask_ $ do
    ptr <- ioPtr
    unsafeFromPtr ptr

unsafeFromPtr :: Ptr C'Mat -> IO Mat
unsafeFromPtr ptr = do
    let deleteMat = [C.funPtr| void deleteMat(cv::Mat * m) { delete m; } |]
    Mat <$> newForeignPtr deleteMat ptr

fromIOEitherPtr :: IO (Either CE.CppException (Ptr C'Mat)) -> IO Mat
fromIOEitherPtr ioPtr = mask_ $ do
    ioPtr >>= \case
        Left ex -> throwIO $ OpenCVCppException ex
        Right ptr
            | ptr == nullPtr -> throwIO OpenCVNullPtrException
            | otherwise -> unsafeFromPtr ptr

withMatPtr :: Mat -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat

printMat :: Mat -> IO ()
printMat mat = do
    withMatPtr mat $ \matPtr ->
        [C.block|void {
          std::cout << *$(cv::Mat * matPtr) << std::endl;
        }|]

isEmpty :: Mat -> IO Bool
isEmpty mat = do
    result <- withMatPtr mat $ \matPtr ->
        [C.exp|bool{ $(cv::Mat * matPtr)->empty() }|]
    return $ result /= 0

checkNonEmptyOrThrow :: String -> Mat -> IO ()
checkNonEmptyOrThrow errMsg mat = do
    empty <- isEmpty mat
    when empty $ throwIO $ OpenCVUnexpectedStatusException errMsg
