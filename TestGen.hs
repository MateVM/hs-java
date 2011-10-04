{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import Java.ClassPath

import qualified Java.Lang
import qualified Java.IO

test :: Generate ()
test = do
  withClassPath $ do
      addDirectory "."

  helloJava <- getClassMethod "./Hello" "hello"

  newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize 1

      aload_ I0
      invokeSpecial Java.Lang.object Java.Lang.objectInit
      i0 RETURN

  hello <- newMethod [ACC_PUBLIC, ACC_STATIC] "hello" [IntType] ReturnsVoid $ do
      setStackSize 8

      getStaticField Java.Lang.system Java.IO.out
      loadString "Здравствуй, мир!"
      invokeVirtual Java.IO.printStream Java.IO.println
      getStaticField Java.Lang.system Java.IO.out
      loadString "Argument: %d\n"
      iconst_1
      allocArray Java.Lang.object
      dup
      iconst_0
      iload_ I0
      invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
      aastore
      invokeVirtual Java.IO.printStream Java.IO.printf
      invokeStatic "Hello" helloJava
      pop
      i0 RETURN

  newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
      setStackSize 1

      iconst_5
      invokeStatic "Test" hello
      i0 RETURN

  return ()

main = do
  testClass <- generate [] "Test" test
  B.writeFile "Test.class" (encodeClass testClass)

