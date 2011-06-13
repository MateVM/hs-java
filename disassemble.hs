{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import qualified Data.ByteString.Lazy as B

import Data.BinaryState
import JVM.Types
import JVM.Converter
import JVM.Assembler

main = do
  args <- getArgs
  case args of
    [clspath] -> do
      cls <- decompileFile clspath
      putStr "Class: "
      B.putStrLn (this cls)
      putStrLn "Methods:"
      forM_ (methods cls) $ \m -> do
        putStr ">> Method "
        B.putStrLn (methodName m)
        case attrByName m "Code" of
          Nothing -> putStrLn "(no code)\n"
          Just bytecode -> let code = decodeS (0 :: Integer) bytecode
                           in  forM_ (codeInstructions code) $ \i -> do
                                 putStr "  "
                                 print i

    _ -> error "Synopsis: disassemble File.class"
