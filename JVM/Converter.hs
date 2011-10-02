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
import JVM.Common
import JVM.Exceptions

-- | Parse .class file data
parseClass :: B.ByteString -> Class Direct
parseClass bstr = convertClass $ decode bstr

-- | Parse class data from file
parseClassFile :: FilePath -> IO (Class Direct)
parseClassFile path = convertClass `fmap` decodeFile path

encodeClass :: (Class Direct) -> B.ByteString
encodeClass cls = encode $ classFile cls

convertClass :: Class File -> Class Direct
convertClass (Class {..}) =
  let pool = constantPoolArray constsPool
      superName = className $ pool ! superClass
  in Class {
      magic = 0xCAFEBABE,
      minorVersion = 0,
      majorVersion = 50,
      constsPoolSize = fromIntegral (M.size pool),
      constsPool = pool,
      accessFlags = convertAccess accessFlags,
      thisClass = className $ pool ! thisClass,
      superClass = if superClass == 0 then "" else superName,
      interfacesCount = interfacesCount,
      interfaces = map (\i -> className $ pool ! i) interfaces,
      classFieldsCount = classFieldsCount,
      classFields = map (convertField pool) classFields,
      classMethodsCount = classMethodsCount,
      classMethods = map (convertMethod pool) classMethods,
      classAttributesCount = classAttributesCount,
      classAttributes = convertAttrs pool classAttributes }

classFile :: Class Direct -> Class File
classFile (Class {..}) = Class {
    magic = 0xCAFEBABE,
    minorVersion = 0,
    majorVersion = 50,
    constsPoolSize = fromIntegral (M.size poolInfo + 1),
    constsPool = poolInfo,
    accessFlags = access2word16 accessFlags,
    thisClass = force "this" $ poolClassIndex poolInfo thisClass,
    superClass = force "super" $ poolClassIndex poolInfo superClass,
    interfacesCount = fromIntegral (length interfaces),
    interfaces = map (force "ifaces" . poolIndex poolInfo) interfaces,
    classFieldsCount = fromIntegral (length classFields),
    classFields = map (fieldInfo poolInfo) classFields,
    classMethodsCount = fromIntegral (length classMethods),
    classMethods = map (methodInfo poolInfo) classMethods,
    classAttributesCount = fromIntegral $ arsize classAttributes,
    classAttributes = to (arlist classAttributes) }
  where
    poolInfo = toCPInfo constsPool
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo poolInfo) pairs)

toCPInfo :: Pool Direct -> Pool File
toCPInfo pool = result
  where
    result = M.map cpInfo pool

    cpInfo :: Constant Direct -> Constant File
    cpInfo (CClass name) = CClass (force "class" $ poolIndex result name)
    cpInfo (CField cls name) =
      CField (force "field a" $ poolClassIndex result cls) (force "field b" $ poolNTIndex result name)
    cpInfo (CMethod cls name) =
      CMethod (force "method a" $ poolClassIndex result cls) (force ("method b: " ++ show name) $ poolNTIndex result name)
    cpInfo (CIfaceMethod cls name) =
      CIfaceMethod (force "iface method a" $ poolIndex result cls) (force "iface method b" $ poolNTIndex result name)
    cpInfo (CString s) = CString (force "string" $ poolIndex result s)
    cpInfo (CInteger x) = CInteger x
    cpInfo (CFloat x) = CFloat x
    cpInfo (CLong x) = CLong (fromIntegral x)
    cpInfo (CDouble x) = CDouble x
    cpInfo (CNameType n t) =
      CNameType (force "name" $ poolIndex result n) (force "type" $ poolIndex result t)
    cpInfo (CUTF8 s) = CUTF8 s
    cpInfo (CUnicode s) = CUnicode s

-- | Find index of given string in the list of constants
poolIndex :: (Throws NoItemInPool e) => Pool File -> B.ByteString -> EM e Word16
poolIndex list name = case findIndex test (M.elems list) of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  return $ fromIntegral $ i+1
  where
    test (CUTF8 s)    | s == name = True
    test (CUnicode s) | s == name = True
    test _                                  = False

-- | Find index of given string in the list of constants
poolClassIndex :: (Throws NoItemInPool e) => Pool File -> B.ByteString -> EM e Word16
poolClassIndex list name = case findIndex checkString (M.elems list) of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  case findIndex (checkClass $ fromIntegral $ i+1) (M.elems list) of
                                     Nothing -> throw (NoItemInPool $ i+1)
                                     Just j  -> return $ fromIntegral $ j+1
  where
    checkString (CUTF8 s)    | s == name = True
    checkString (CUnicode s) | s == name = True
    checkString _                                  = False

    checkClass i (CClass x) | i == x = True
    checkClass _ _                           = False

poolNTIndex list x@(NameType n t) = do
    ni <- poolIndex list n
    ti <- poolIndex list (byteString t)
    case findIndex (check ni ti) (M.elems list) of
      Nothing -> throw (NoItemInPool x)
      Just i  -> return $ fromIntegral (i+1)
  where
    check ni ti (CNameType n' t')
      | (ni == n') && (ti == t') = True
    check _ _ _                  = False

fieldInfo :: Pool File -> Field Direct -> Field File
fieldInfo pool (Field {..}) = Field {
    fieldAccessFlags = access2word16 fieldAccessFlags,
    fieldName = force "field name" $ poolIndex pool fieldName,
    fieldSignature = force "signature" $ poolIndex pool (encode fieldSignature),
    fieldAttributesCount = fromIntegral (arsize fieldAttributes),
    fieldAttributes = to (arlist fieldAttributes) }
  where
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo pool) pairs)

methodInfo :: Pool File -> Method Direct -> Method File
methodInfo pool (Method {..}) = Method {
    methodAccessFlags = access2word16 methodAccessFlags,
    methodName = force "method name" $ poolIndex pool methodName,
    methodSignature = force "method sig" $ poolIndex pool (encode methodSignature),
    methodAttributesCount = fromIntegral (arsize methodAttributes),
    methodAttributes = to (arlist methodAttributes) }
  where
    to :: [(B.ByteString, B.ByteString)] -> Attributes File
    to pairs = AP (map (attrInfo pool) pairs)

attrInfo :: Pool File -> (B.ByteString, B.ByteString) -> Attribute
attrInfo pool (name, value) = Attribute {
  attributeName = force "attr name" $ poolIndex pool name,
  attributeLength = fromIntegral (B.length value),
  attributeValue = value }

constantPoolArray :: Pool File -> Pool Direct
constantPoolArray ps = pool
  where
    pool :: Pool Direct
    pool = M.map convert ps

    n = fromIntegral $ M.size ps

    convertNameType :: (HasSignature a) => Word16 -> NameType a
    convertNameType i =
      let (CNameType n s) = pool ! i
      in  NameType n (decode s)

    convert (CClass i) = CClass $ getString $ pool ! i
    convert (CField i j) = CField (className $ pool ! i) (convertNameType j)
    convert (CMethod i j) = CMethod (className $ pool ! i) (convertNameType j)
    convert (CIfaceMethod i j) = CIfaceMethod (className $ pool ! i) (convertNameType j)
    convert (CString i) = CString $ getString $ pool ! i
    convert (CInteger x) = CInteger x
    convert (CFloat x)   = CFloat x
    convert (CLong x)    = CLong (fromIntegral x)
    convert (CDouble x)  = CDouble x
    convert (CNameType i j) = CNameType (getString $ pool ! i) (getString $ pool ! j)
    convert (CUTF8 bs) = CUTF8 bs
    convert (CUnicode bs) = CUnicode bs

convertAccess :: AccessFlags File -> AccessFlags Direct
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

access2word16 :: AccessFlags Direct -> AccessFlags File
access2word16 fs = bitsOr $ map toBit $ S.toList fs
  where
    bitsOr = foldl (.|.) 0
    toBit f = 1 `shiftL` (fromIntegral $ fromEnum f)

convertField :: Pool Direct -> Field File -> Field Direct
convertField pool (Field {..}) = Field {
  fieldAccessFlags = convertAccess fieldAccessFlags,
  fieldName = getString $ pool ! fieldName,
  fieldSignature = decode $ getString $ pool ! fieldSignature,
  fieldAttributesCount = fromIntegral (apsize fieldAttributes),
  fieldAttributes = convertAttrs pool fieldAttributes }

convertMethod :: Pool Direct -> Method File -> Method Direct
convertMethod pool (Method {..}) = Method {
  methodAccessFlags = convertAccess methodAccessFlags,
  methodName = getString $ pool ! methodName,
  methodSignature = decode $ getString $ pool ! methodSignature,
  methodAttributesCount = fromIntegral (apsize methodAttributes),
  methodAttributes = convertAttrs pool methodAttributes }

convertAttrs :: Pool Direct -> Attributes File -> Attributes Direct
convertAttrs pool (AP attrs) = AR (M.fromList $ map go attrs)
  where
    go :: Attribute -> (B.ByteString, B.ByteString)
    go (Attribute {..}) = (getString $ pool ! attributeName,
                           attributeValue)

-- | Try to get class method by name
methodByName :: Class Direct -> B.ByteString -> Maybe (Method Direct)
methodByName cls name =
  find (\m -> methodName m == name) (classMethods cls)

-- | Try to get object attribute by name
attrByName :: (HasAttributes a) => a Direct -> B.ByteString -> Maybe B.ByteString
attrByName x name =
  let (AR m) = attributes x
  in  M.lookup name m

-- | Try to get Code for class method (no Code for interface methods)
methodCode :: Class Direct
           -> B.ByteString       -- ^ Method name
           -> Maybe B.ByteString
methodCode cls name = do
  method <- methodByName cls name
  attrByName method "Code"

