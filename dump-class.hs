{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Array
import Data.Binary
import System.Environment
import qualified Data.ByteString.Lazy as B
import Text.Printf
import qualified Data.Map as M

import JVM.Types
import JVM.ClassFile
import JVM.Converter
import JVM.Dump

main = do
  args <- getArgs
  case args of
    [clspath] -> do
      clsFile <- decodeFile clspath
      putStrLn $ showListIx $ M.elems $ constsPool (clsFile :: Class Pointers)
      cls <- parseClassFile clspath
      dumpClass cls
    _ -> error "Synopsis: dump-class File.class"

