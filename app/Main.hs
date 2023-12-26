module Main where

import Control.Monad
import Data.ByteString qualified as S
import Data.ByteString.Char8 qualified as S8
import OpenCV.Core.Mat
import OpenCV.Extra.ImgHash
import OpenCV.ImgCodecs
import System.Environment

main :: IO ()
main = do
    files <- getArgs
    forM_ files $ \file -> do
        mat <- newMatFromFile (S8.pack file)
        phash <- computePHash mat
        printMat phash

    forM_ files $ \file -> do
        bs <- S.readFile file
        mat <- imDecode bs imreadUnchanged
        phash <- computePHash mat
        printMat phash
