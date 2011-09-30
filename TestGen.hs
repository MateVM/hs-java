{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B

import JVM.Types
import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Generator
import JVM.Dump

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
      i0 $ ALOAD_ I0
      i1 INVOKESPECIAL (CMethod "java/lang/Object" initNT)
      i0 RETURN

  newMethod [ACC_PUBLIC, ACC_STATIC] "main" [Array Nothing $ ObjectType "java/lang/String"] ReturnsVoid $ do
      i0 ICONST_5
      i1 INVOKESTATIC (CMethod "Test" helloNT)
      i0 RETURN

  newMethod [ACC_PUBLIC, ACC_STATIC] "hello" [IntType] ReturnsVoid $ do
      i1 GETSTATIC (CField "java/lang/System" outNT)
      i8 LDC1 (CString "Здравствуй, мир!")
      i1 INVOKEVIRTUAL (CMethod "java/io/PrintStream" printlnNT)
      i1 GETSTATIC (CField "java/lang/System" outNT)
      i8 LDC1 (CString "Argument: %d\n")
      i0 ICONST_1
      i1 ANEWARRAY (CClass "java/lang/Object")
      i0 DUP
      i0 ICONST_0
      i0 (ILOAD_ I0)
      i1 INVOKESTATIC (CMethod "java/lang/Integer" valueOfNT)
      i0 AASTORE
      i1 INVOKEVIRTUAL (CMethod "java/io/PrintStream" printfNT)
      i0 POP
      i0 RETURN

testClass = generate "Test" test

main = do
  B.writeFile "Test.class" (encodeClass testClass)

