{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Extra.ImgHash where

import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as CE
import OpenCV.Core.Mat qualified as Mat
import OpenCV.Internal.InlineCpp (cvCtx)

C.context cvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/img_hash.hpp"

computePHash :: Mat.Mat -> IO Mat.Mat
computePHash img = do
    out <- Mat.newEmptyMat
    Mat.withMatPtr out $ \outPtr ->
        Mat.withMatPtr img $ \imgPtr ->
            [CE.catchBlock|
              cv::Ptr<cv::img_hash::ImgHashBase> f = cv::img_hash::PHash::create();
              f->compute(*$(cv::Mat * imgPtr), *$(cv::Mat * outPtr));
            |]
    Mat.checkNonEmptyOrThrow "failed to calculate image hash" out
    return out
