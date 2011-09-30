{-# LANGUAGE OverloadedStrings #-}
module Java.Lang where

import Data.String

import JVM.Common ()  -- import instances only
import JVM.ClassFile

objectClass = ObjectType object
stringClass = ObjectType string
integerClass = ObjectType integer
systemClass = ObjectType system

object :: IsString s => s
object = "java/lang/Object"

string :: IsString s => s
string = "java/lang/String"

integer :: IsString s => s
integer = "java/lang/Integer"

system :: IsString s => s
system = "java/lang/System"

objectInit :: NameType Method
objectInit = NameType "<init>" $ MethodSignature [] ReturnsVoid

valueOfInteger :: NameType Method
valueOfInteger = NameType "valueOf" $ MethodSignature [IntType] (Returns Java.Lang.integerClass)

