module Main where

import OpenCV.Extra.ImgHash
import System.Environment
import Control.Monad
import Data.ByteString.Char8 qualified as S8

main :: IO ()
main = do
    files <- getArgs
    forM_ files $ \file -> do
        mat <- newMatFromFile $ S8.pack file
        phash <- computePHash mat
        printMat phash
