{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, RecordWildCards, OverloadedStrings #-}
-- | Functions to convert from low-level .class format representation and
-- high-level Java classes, methods etc representation
module JVM.Converter
  (decompile, decompileFile,
   convertClass,
   methodByName,
   attrByName,
   methodCode
  )
  where

import Data.List
import Data.Word
import Data.Bits
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

import JVM.ClassFile
import JVM.Types

-- | Parse .class file data
decompile :: B.ByteString -> Class
decompile bstr = convertClass $ decode bstr

-- | Parse class data from file
decompileFile :: FilePath -> IO Class
decompileFile path = convertClass `fmap` decodeFile path

convertClass :: ClassFile -> Class
convertClass (ClassFile {..}) =
  let pool = constantPoolArray constsPool
      superName = className $ pool ! superClass
  in Class {
      constantPool = pool,
      classAccess = convertAccess accessFlags,
      this = className $ pool ! thisClass,
      super = if superClass == 0 then Nothing else Just superName,
      implements = map (\i -> className $ pool ! i) interfaces,
      fields = map (convertField pool) classFields,
      methods = map (convertMethod pool) classMethods,
      classAttrs = convertAttrs pool classAttributes }

constantPoolArray :: [CpInfo] -> Pool
constantPoolArray list = pool
  where
    pool :: Pool
    pool = listArray (1,n) $ map convert list
    n = fromIntegral $ length list

    convertNameType :: (HasSignature a, Binary (Signature a)) => Word16 -> NameType a
    convertNameType i =
      let (CNameType n s) = pool ! i
      in  NameType n (decode s)

    convert (CONSTANT_Class i) = CClass $ getString $ pool ! i
    convert (CONSTANT_Fieldref i j) = CField (className $ pool ! i) (convertNameType j)
    convert (CONSTANT_Methodref i j) = CMethod (className $ pool ! i) (convertNameType j)
    convert (CONSTANT_InterfaceMethodref i j) = CIfaceMethod (className $ pool ! i) (convertNameType j)
    convert (CONSTANT_String i) = CString $ getString $ pool ! i
    convert (CONSTANT_Integer x) = CInteger x
    convert (CONSTANT_Float x)   = CFloat x
    convert (CONSTANT_Long x)    = CLong (fromIntegral x)
    convert (CONSTANT_Double x)  = CDouble x
    convert (CONSTANT_NameAndType i j) = CNameType (getString $ pool ! i) (getString $ pool ! j)
    convert (CONSTANT_Utf8 _ bs) = CUTF8 bs
    convert (CONSTANT_Unicode _ bs) = CUnicode bs

convertAccess :: Word16 -> Access
convertAccess w = S.fromList $ concat $ zipWith (\i f -> if testBit w i then [f] else []) [0..] $ [
   ACC_PUBLIC,
   ACC_PRIVATE,
   ACC_PROTECTED,
   ACC_STATIC,
   ACC_FINAL,
   ACC_SYNCHRONIZED,
   ACC_VOLATILE,
   ACC_TRANSIENT,
   ACC_NATIVE,
   ACC_INTERFACE,
   ACC_ABSTRACT ]

convertField :: Pool -> FieldInfo -> Field
convertField pool (FieldInfo {..}) = Field {
  fieldAccess = convertAccess fieldAccessFlags,
  fieldName = getString $ pool ! fieldNameIndex,
  fieldSignature = decode $ getString $ pool ! fieldSignatureIndex,
  fieldAttrs = convertAttrs pool fieldAttributes }

convertMethod :: Pool -> MethodInfo -> Method
convertMethod pool (MethodInfo {..}) = Method {
  methodAccess = convertAccess methodAccessFlags,
  methodName = getString $ pool ! methodNameIndex,
  methodSignature = decode $ getString $ pool ! methodSignatureIndex,
  methodAttrs = convertAttrs pool methodAttributes }

convertAttrs :: Pool -> [AttributeInfo] -> Attributes
convertAttrs pool attrs = M.fromList $ map go attrs
  where
    go (AttributeInfo {..}) = (getString $ pool ! attributeName,
                               attributeValue)

-- | Try to get class method by name
methodByName :: Class -> B.ByteString -> Maybe Method
methodByName cls name =
  find (\m -> methodName m == name) (methods cls)

-- | Try to get object attribute by name
attrByName :: (HasAttributes a) => a -> B.ByteString -> Maybe B.ByteString
attrByName x name = M.lookup name (attributes x)

-- | Try to get Code for class method (no Code for interface methods)
methodCode :: Class
           -> B.ByteString       -- ^ Method name
           -> Maybe B.ByteString
methodCode cls name = do
  method <- methodByName cls name
  attrByName method "Code"

