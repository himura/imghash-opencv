{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Extra.ImgHash where

import Control.Exception (mask_)
import Data.ByteString qualified as S
import Foreign.ForeignPtr
import Foreign.Ptr
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Unsafe qualified as CU
import OpenCV.C.Inline

C.context cvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgcodecs.hpp"
C.include "opencv2/img_hash.hpp"
C.include "<iostream>"
C.using "namespace cv"
C.using "namespace cv::img_hash"

newtype Mat = Mat {unMat :: ForeignPtr C'Mat}

newEmptyMat :: IO Mat
newEmptyMat = mask_ $ do
    ptr <- [CU.exp|Mat*{ new Mat() }|]
    let deleteMat = [C.funPtr| void deleteMat(Mat * m) { delete m; } |]
    Mat <$> newForeignPtr deleteMat ptr

newMatFromFile :: S.ByteString -> IO (Maybe Mat)
newMatFromFile bs = mask_ $ do
    ptr <-
        [CU.block|Mat*{
          try {
            Mat m = imread($bs-cstr:bs);
            if (m.empty()) {
              return NULL;
            }
            return new Mat(m);
          } catch (const cv::Exception & e) {
            return NULL;
          }
        }|]
    if ptr == nullPtr
        then return Nothing
        else do
            let deleteMat = [C.funPtr| void deleteMat(Mat * m) { delete m; } |]
            Just . Mat <$> newForeignPtr deleteMat ptr

withMatPtr :: Mat -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat

computePHash :: Mat -> IO Mat
computePHash img = do
    out <- newEmptyMat
    withMatPtr out $ \outPtr ->
        withMatPtr img $ \imgPtr ->
            [C.block|void {
              Ptr<ImgHashBase> f = PHash::create();
              f->compute(*$(Mat * imgPtr), *$(Mat * outPtr));
            }|]
    return out

printMat :: Mat -> IO ()
printMat mat = do
    withMatPtr mat $ \matPtr ->
        [C.block|void {
          std::cout << *$(Mat * matPtr) << std::endl;
        }|]
