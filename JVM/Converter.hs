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
parseClass :: B.ByteString -> Class Resolved
parseClass bstr = convertClass $ decode bstr

-- | Parse class data from file
parseClassFile :: FilePath -> IO (Class Resolved)
parseClassFile path = convertClass `fmap` decodeFile path

encodeClass :: (Class Resolved) -> B.ByteString
encodeClass cls = encode $ classFile cls

convertClass :: Class Pointers -> Class Resolved
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

classFile :: Class Resolved -> Class Pointers
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
    classAttributesCount = fromIntegral (M.size classAttributes),
    classAttributes = map (attrInfo poolInfo) (M.assocs classAttributes) }
  where
    poolInfo = toCPInfo constsPool

toCPInfo :: Pool Resolved -> Pool Pointers
toCPInfo pool = result
  where
    result = M.map cpInfo pool

    cpInfo :: Constant Resolved -> Constant Pointers
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
    cpInfo (CUTF8 s) = CUTF8 (fromIntegral $ B.length s) s
    cpInfo (CUnicode s) = CUnicode (fromIntegral $ B.length s) s

-- | Find index of given string in the list of constants
poolIndex :: (Throws NoItemInPool e) => Pool Pointers -> B.ByteString -> EM e Word16
poolIndex list name = case findIndex test list of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  return $ fromIntegral $ i+1
  where
    test (CUTF8 s)    | s == name = True
    test (CUnicode s) | s == name = True
    test _                                  = False

-- | Find index of given string in the list of constants
poolClassIndex :: (Throws NoItemInPool e) => Pool Pointers -> B.ByteString -> EM e Word16
poolClassIndex list name = case findIndex checkString list of
                        Nothing -> throw (NoItemInPool name)
                        Just i ->  case findIndex (checkClass $ fromIntegral $ i+1) list of
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
    case findIndex (check ni ti) list of
      Nothing -> throw (NoItemInPool x)
      Just i  -> return $ fromIntegral (i+1)
  where
    check ni ti (CNameType n' t')
      | (ni == n') && (ti == t') = True
    check _ _ _                  = False

fieldInfo :: Pool Pointers -> Field Resolved -> Field Pointers
fieldInfo pool (Field {..}) = Field {
  fieldAccessFlags = access2word16 fieldAccessFlags,
  fieldName = force "field name" $ poolIndex pool fieldName,
  fieldSignature = force "signature" $ poolIndex pool (encode fieldSignature),
  fieldAttributesCount = fromIntegral (M.size fieldAttributes),
  fieldAttributes = map (attrInfo pool) (M.assocs fieldAttributes) }

methodInfo :: Pool Pointers -> Method Resolved -> Method Pointers
methodInfo pool (Method {..}) = Method {
  methodAccessFlags = access2word16 methodAccessFlags,
  methodName = force "method name" $ poolIndex pool methodName,
  methodSignature = force "method sig" $ poolIndex pool (encode methodSignature),
  methodAttributesCount = fromIntegral (M.size methodAttributes),
  methodAttributes = map (attrInfo pool) (M.assocs methodAttributes) }

attrInfo :: Pool Pointers -> (B.ByteString, B.ByteString) -> Attributes Pointers
attrInfo pool (name, value) = Attribute {
  attributeName = force "attr name" $ poolIndex pool name,
  attributeLength = fromIntegral (B.length value),
  attributeValue = value }

constantPoolArray :: Pool Pointers -> Pool Resolved
constantPoolArray ps = pool
  where
    pool :: Pool
    pool = M.map convert ps

    n = fromIntegral $ length ps

    convertNameType :: (HasSignature a, Binary (Signature a)) => Word16 -> NameType a
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
    convert (CUTF8 _ bs) = CUTF8 bs
    convert (CUnicode _ bs) = CUnicode bs

convertAccess :: AccessFlags Pointers -> AccessFlags Resolved
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

access2word16 :: AccessFlags Resolved -> AccessFlags Pointers
access2word16 fs = bitsOr $ map toBit $ S.toList fs
  where
    bitsOr = foldl (.|.) 0
    toBit f = 1 `shiftL` (fromIntegral $ fromEnum f)

convertField :: Pool Resolved -> Field Pointers -> Field Resolved
convertField pool (Field {..}) = Field {
  fieldAccessFlags = convertAccess fieldAccessFlags,
  fieldName = getString $ pool ! fieldName,
  fieldSignature = decode $ getString $ pool ! fieldSignature,
  fieldAttributes = convertAttrs pool fieldAttributes }

convertMethod :: Pool Resolved -> Method Pointers -> Method Resolved
convertMethod pool (Method {..}) = Method {
  methodAccessFlags = convertAccess methodAccessFlags,
  methodName = getString $ pool ! methodName,
  methodSignature = decode $ getString $ pool ! methodSignature,
  methodAttributes = convertAttrs pool methodAttributes }

convertAttrs :: Pool Resolved -> Attributes Pointers -> Attributes Resolved
convertAttrs pool attrs = M.fromList $ map go attrs
  where
    go (Attribute {..}) = (getString $ pool ! attributeName,
                           attributeValue)

-- | Try to get class method by name
methodByName :: Class Resolved -> B.ByteString -> Maybe (Method Resolved)
methodByName cls name =
  find (\m -> methodName m == name) (classMethods cls)

-- | Try to get object attribute by name
attrByName :: (HasAttributes a) => a Resolved -> B.ByteString -> Maybe B.ByteString
attrByName x name = M.lookup name (attributes x)

-- | Try to get Code for class method (no Code for interface methods)
methodCode :: Class Resolved
           -> B.ByteString       -- ^ Method name
           -> Maybe B.ByteString
methodCode cls name = do
  method <- methodByName cls name
  attrByName method "Code"

