{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgCodecs where

import Data.Bits
import Data.ByteString qualified as S
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as CE
import OpenCV.Core.Mat qualified as Mat
import OpenCV.Internal.InlineCpp (cvCtx)

C.context cvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgcodecs.hpp"

newtype ImreadModes = ImreadModes {unImreadModes :: C.CInt}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Num)
instance Semigroup ImreadModes where
    ImreadModes a <> ImreadModes b = ImreadModes $ a .|. b
instance Monoid ImreadModes where
    mempty = ImreadModes 0

imreadUnchanged
    , imreadGrayscale
    , imreadColor
    , imreadAnydepth
    , imreadAnycolor
    , imreadLoadGdal
    , imreadReducedGrayscale2
    , imreadReducedColor2
    , imreadReducedGrayscale4
    , imreadReducedColor4
    , imreadReducedGrayscale8
    , imreadReducedColor8
    , imreadIgnoreOrientation
        :: ImreadModes
imreadUnchanged = ImreadModes (-1) -- If Set, Return The Loaded Image As Is (With Alpha Channel, Otherwise It Gets Cropped). Ignore Exif Orientation.
imreadGrayscale = ImreadModes 0 -- If Set, Always Convert Image To The Single Channel Grayscale Image (Codec Internal Conversion).
imreadColor = ImreadModes 1 -- If Set, Always Convert Image To The 3 Channel Bgr Color Image.
imreadAnydepth = ImreadModes 2 -- If Set, Return 16-Bit/32-Bit Image When The Input Has The Corresponding Depth, Otherwise Convert It To 8-Bit.
imreadAnycolor = ImreadModes 4 -- If Set, The Image Is Read In Any Possible Color Format.
imreadLoadGdal = ImreadModes 8 -- If Set, Use The Gdal Driver For Loading The Image.
imreadReducedGrayscale2 = ImreadModes 16 -- If Set, Always Convert Image To The Single Channel Grayscale Image And The Image Size Reduced 1/2.
imreadReducedColor2 = ImreadModes 17 -- If Set, Always Convert Image To The 3 Channel Bgr Color Image And The Image Size Reduced 1/2.
imreadReducedGrayscale4 = ImreadModes 32 -- If Set, Always Convert Image To The Single Channel Grayscale Image And The Image Size Reduced 1/4.
imreadReducedColor4 = ImreadModes 33 -- If Set, Always Convert Image To The 3 Channel Bgr Color Image And The Image Size Reduced 1/4.
imreadReducedGrayscale8 = ImreadModes 64 -- If Set, Always Convert Image To The Single Channel Grayscale Image And The Image Size Reduced 1/8.
imreadReducedColor8 = ImreadModes 65 -- If Set, Always Convert Image To The 3 Channel Bgr Color Image And The Image Size Reduced 1/8.
imreadIgnoreOrientation = ImreadModes 128 -- If set, do not rotate the image according to EXIF's orientation flag.

newMatFromFile :: S.ByteString -> IO Mat.Mat
newMatFromFile bs = do
    mat <-
        Mat.fromIOEitherPtr
            [CE.tryBlock|cv::Mat*{
              return new cv::Mat(cv::imread($bs-cstr:bs));
            }|]
    Mat.checkNonEmptyOrThrow "newMatFromFile: imread failed" mat
    return mat

imDecode :: S.ByteString -> ImreadModes -> IO Mat.Mat
imDecode buf (ImreadModes modes) = do
    mat <- Mat.newEmptyMat
    Mat.withMatPtr mat $ \matPtr ->
        [CE.catchBlock|
          cv::_InputArray ia = cv::_InputArray($bs-ptr:buf, $bs-len:buf);
          cv::imdecode(ia, $(int modes), $(cv::Mat* matPtr));
        |]
    Mat.checkNonEmptyOrThrow "imDecode: imdecode failed" mat
    return mat
