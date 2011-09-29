{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Array
import System.Environment
import qualified Data.ByteString.Lazy as B
import Text.Printf

import JVM.Converter
import JVM.Dump

main = do
  args <- getArgs
  case args of
    [clspath] -> do
      cls <- parseClassFile clspath
      dumpClass cls
    _ -> error "Synopsis: dump-class File.class"

