module Main where

import OpenCV.Extra.ImgHash
import System.Environment
import Control.Monad
import Data.ByteString.Char8 qualified as S8

main :: IO ()
main = do
    files <- getArgs
    forM_ files $ \file -> do
        newMatFromFile (S8.pack file) >>= \case
            Nothing -> putStrLn $ "failed to open: " ++ file
            Just mat -> do
                phash <- computePHash mat
                printMat phash
