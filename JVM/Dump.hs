{-# LANGUAGE OverloadedStrings #-}
module JVM.Dump where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Text.Printf

import JVM.Types
import JVM.Converter
import JVM.Assembler

dumpClass :: Class -> IO ()
dumpClass cls = do
    putStr "Class: "
    B.putStrLn (this cls)
    putStrLn "Constants pool:"
    forM_ (M.assocs $ constantPool cls) $ \(i, c) ->
      putStrLn $ printf "  #%d:\t%s" i (show c)
    putStrLn "Methods:"
    forM_ (methods cls) $ \m -> do
      putStr ">> Method "
      B.putStr (methodName m)
      print (methodSignature m)
      case attrByName m "Code" of
        Nothing -> putStrLn "(no code)\n"
        Just bytecode -> let code = decodeMethod bytecode
                         in  forM_ (codeInstructions code) $ \i -> do
                               putStr "  "
                               print i

