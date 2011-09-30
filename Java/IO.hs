{-# LANGUAGE OverloadedStrings #-}
module Java.IO where

import Data.String

import JVM.ClassFile
import JVM.Types

import qualified Java.Lang

printStream :: IsString s => s
printStream = "java/io/PrintStream"

printStreamClass = ObjectType printStream

println :: NameType Method
println = NameType "println" $ MethodSignature [Java.Lang.stringClass] ReturnsVoid

out :: NameType Field
out = NameType "out" printStreamClass

printf :: NameType Method
printf =
  NameType "printf" $ MethodSignature [Java.Lang.stringClass,
                                       Array Nothing Java.Lang.objectClass] (Returns printStreamClass)

