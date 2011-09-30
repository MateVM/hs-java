{-# LANGUAGE OverloadedStrings #-}

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
import JVM.Assembler
import JVM.Dump

main = do
  args <- getArgs
  case args of
    [clspath,outpath] -> do
      cls <- parseClassFile clspath
      clsfile <- decodeFile clspath :: IO (Class Pointers)
      dumpClass cls
      putStrLn $ "Source pool:\n" ++ showListIx (M.elems $ constsPool clsfile)
      let result = classFile cls
      putStrLn $ "Result pool:\n" ++ showListIx (M.elems $ constsPool result)
      B.writeFile outpath (encodeClass cls)

    _ -> error "Synopsis: rebuild-class File.class Output.class"
