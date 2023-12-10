{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Extra.ImgHash where

import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as C
import OpenCV.C.Inline
import OpenCV.Core.Mat

C.context cvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/img_hash.hpp"
C.using "namespace cv"
C.using "namespace cv::img_hash"

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
