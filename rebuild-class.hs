{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Array
import Data.Binary
import System.Environment
import qualified Data.ByteString.Lazy as B
import Text.Printf

import JVM.Types
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler

main = do
  args <- getArgs
  case args of
    [clspath,outpath] -> do
      cls <- parseClassFile clspath
      clsfile <- decodeFile clspath :: IO ClassFile
      putStr "Class: "
      B.putStrLn (this cls)
      putStrLn "Constants pool:"
      forM_ (assocs $ constantPool cls) $ \(i, c) ->
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
      putStrLn $ "Source pool:\n" ++ showListIx (constsPool clsfile)
      let result = classFile cls
      putStrLn $ "Result pool:\n" ++ showListIx (constsPool result)
      B.writeFile outpath (encodeClass cls)

    _ -> error "Synopsis: rebuild-class File.class Output.class"
