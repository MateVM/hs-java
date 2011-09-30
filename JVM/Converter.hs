{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, RecordWildCards, OverloadedStrings #-}
-- | Functions to convert from low-level .class format representation and
-- high-level Java classes, methods etc representation
module JVM.Converter
  (parseClass, parseClassFile,
   convertClass, classFile,
   encodeClass,
   methodByName,
   attrByName,
   methodCode
  )
  where

import Control.Monad.Exception
import Data.List
import Data.Word
import Data.Bits
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Map as M

import JVM.ClassFile
import JVM.Types
import JVM.Exceptions

-- | Parse .class file data
parseClass :: B.ByteString -> Class
parseClass bstr = convertClass $ decode bstr

-- | Parse class data from file
parseClassFile :: FilePath -> IO Class
parseClassFile path = convertClass `fmap` decodeFile path

encodeClass :: Class -> B.ByteString
encodeClass cls = encode $ classFile cls

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

classFile :: Class -> ClassFile
classFile (Class {..}) = ClassFile {
    magic = 0xCAFEBABE,
    minorVersion = 0,
    majorVersion = 50,
    constsPoolSize = fromIntegral (length poolInfo + 1),
    constsPool = poolInfo,
    accessFlags = access2word16 classAccess,
    thisClass = force "this" $ poolClassIndex poolInfo this,
    superClass = case super of
                  Just s -> force "super" $ poolClassIndex poolInfo s
                  Nothing -> 0,
    interfacesCount = fromIntegral (length implements),
    interfaces = map (force "ifaces" . poolIndex poolInfo) implements,
    classFieldsCount = fromIntegral (length fields),
    classFields = map (fieldInfo poolInfo) fields,
    classMethodsCount = fromIntegral (length methods),
    classMethods = map (methodInfo poolInfo) methods,
    classAttributesCount = fromIntegral (M.size classAttrs),
    classAttributes = map (attrInfo poolInfo) (M.assocs classAttrs) }
  where
    poolInfo = toCPInfo constantPool

toCPInfo :: Pool -> [CpInfo]
toCPInfo pool = result
  where
    result = map cpInfo $ M.elems pool

    cpInfo (CClass name) = CONSTANT_Class (force "class" $ poolIndex result name)
    cpInfo (CField cls name) =
      CONSTANT_Fieldref (force "field a" $ poolClassIndex result cls) (force "field b" $ poolNTIndex result name)
    cpInfo (CMethod cls name) =
      CONSTANT_Methodref (force "method a" $ poolClassIndex result cls) (force ("method b: " ++ show name) $ poolNTIndex result name)
    cpInfo (CIfaceMethod cls name) =
      CONSTANT_InterfaceMethodref (force "iface method a" $ poolIndex result cls) (force "iface method b" $ poolNTIndex result name)
    cpInfo (CString s) = CONSTANT_String (force "string" $ poolIndex result s)
    cpInfo (CInteger x) = CONSTANT_Integer x
    cpInfo (CFloat x) = CONSTANT_Float x
    cpInfo (CLong x) = CONSTANT_Long (fromIntegral x)
    cpInfo (CDouble x) = CONSTANT_Double x
    cpInfo (CNameType n t) =
      CONSTANT_NameAndType (force "name" $ poolIndex result n) (force "type" $ poolIndex result t)
    cpInfo (CUTF8 s) = CONSTANT_Utf8 (fromIntegral $ B.length s) s
    cpInfo (CUnicode s) = CONSTANT_Unicode (fromIntegral $ B.length s) s

-- | Find index of given string in the list of constants
poolIndex :: (Throws NoItemInPool e) => [CpInfo] -> B.ByteString -> EM e Word16
poolIndex list name = case findIndex test list of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  return $ fromIntegral $ i+1
  where
    test (CONSTANT_Utf8 _ s)    | s == name = True
    test (CONSTANT_Unicode _ s) | s == name = True
    test _                                  = False

-- | Find index of given string in the list of constants
poolClassIndex :: (Throws NoItemInPool e) => [CpInfo] -> B.ByteString -> EM e Word16
poolClassIndex list name = case findIndex checkString list of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  case findIndex (checkClass $ fromIntegral $ i+1) list of
                                     Nothing -> throw (NoItemInPool $ i+1)
                                     Just j  -> return $ fromIntegral $ j+1
  where
    checkString (CONSTANT_Utf8 _ s)    | s == name = True
    checkString (CONSTANT_Unicode _ s) | s == name = True
    checkString _                                  = False

    checkClass i (CONSTANT_Class x) | i == x = True
    checkClass _ _                           = False

poolNTIndex list x@(NameType n t) = do
    ni <- poolIndex list n
    ti <- poolIndex list (byteString t)
    case findIndex (check ni ti) list of
      Nothing -> throw (NoItemInPool x)
      Just i  -> return $ fromIntegral (i+1)
  where
    check ni ti (CONSTANT_NameAndType n' t')
      | (ni == n') && (ti == t') = True
    check _ _ _                  = False

fieldInfo :: [CpInfo] -> Field -> FieldInfo
fieldInfo pool (Field {..}) = FieldInfo {
  fieldAccessFlags = access2word16 fieldAccess,
  fieldNameIndex = force "field name" $ poolIndex pool fieldName,
  fieldSignatureIndex = force "signature" $ poolIndex pool (encode fieldSignature),
  fieldAttributesCount = fromIntegral (M.size fieldAttrs),
  fieldAttributes = map (attrInfo pool) (M.assocs fieldAttrs) }

methodInfo :: [CpInfo] -> Method -> MethodInfo
methodInfo pool (Method {..}) = MethodInfo {
  methodAccessFlags = access2word16 methodAccess,
  methodNameIndex = force "method name" $ poolIndex pool methodName,
  methodSignatureIndex = force "method sig" $ poolIndex pool (encode methodSignature),
  methodAttributesCount = fromIntegral (M.size methodAttrs),
  methodAttributes = map (attrInfo pool) (M.assocs methodAttrs) }

attrInfo :: [CpInfo] -> (B.ByteString, B.ByteString) -> AttributeInfo
attrInfo pool (name, value) = AttributeInfo {
  attributeName = force "attr name" $ poolIndex pool name,
  attributeLength = fromIntegral (B.length value),
  attributeValue = value }

constantPoolArray :: [CpInfo] -> Pool
constantPoolArray list = pool
  where
    pool :: Pool
    pool = M.fromList $ zip [1..] $ map convert list
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

access2word16 :: Access -> Word16
access2word16 fs = bitsOr $ map toBit $ S.toList fs
  where
    bitsOr = foldl (.|.) 0
    toBit f = 1 `shiftL` (fromIntegral $ fromEnum f)

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

