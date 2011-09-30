{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B

import JVM.Types
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Generator
import JVM.Generator.Instructions

initNT :: NameType Method
initNT = NameType "<init>" $ MethodSignature [] ReturnsVoid

helloNT :: NameType Method
helloNT = NameType "hello" $ MethodSignature [IntType] ReturnsVoid

printlnNT :: NameType Method
printlnNT = NameType "println" $ MethodSignature [ObjectType "java/lang/String"] ReturnsVoid

outNT :: NameType Field
outNT = NameType "out" $ ObjectType "java/io/PrintStream"

valueOfNT :: NameType Method
valueOfNT = NameType "valueOf" $ MethodSignature [IntType] (Returns $ ObjectType "java/lang/Integer")

printfNT :: NameType Method
printfNT = NameType "printf" $ MethodSignature [ObjectType "java/lang/String",
                                                Array Nothing $ ObjectType "java/lang/Object"] (Returns $ ObjectType "java/io/PrintStream")

test :: Generate ()
test = do
  newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
      aload_ I0
      invokeSpecial "java/lang/Object" initNT
      i0 RETURN

  newMethod [ACC_PUBLIC, ACC_STATIC] "main" [Array Nothing $ ObjectType "java/lang/String"] ReturnsVoid $ do
      iconst_5
      invokeStatic "Test" helloNT
      i0 RETURN

  newMethod [ACC_PUBLIC, ACC_STATIC] "hello" [IntType] ReturnsVoid $ do
      getStaticField "java/lang/System" outNT
      loadString "Здравствуй, мир!"
      invokeVirtual "java/io/PrintStream" printlnNT
      getStaticField "java/lang/System" outNT
      loadString "Argument: %d\n"
      iconst_1
      allocArray "java/lang/Object"
      dup
      iconst_0
      iload_ I0
      invokeStatic "java/lang/Integer" valueOfNT
      aastore
      invokeVirtual "java/io/PrintStream" printfNT
      pop
      i0 RETURN

testClass = generate "Test" test

main = do
  B.writeFile "Test.class" (encodeClass testClass)

