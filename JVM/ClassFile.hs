{-# LANGUAGE RecordWildCards, BangPatterns #-}
-- | This module declares (low-level) data types for Java .class files
-- structures, and Binary instances to read/write them.
module JVM.ClassFile where

import Control.Monad
import Control.Applicative
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as B

-- | Read one-byte Char
getChar8 :: Get Char
getChar8 = do
  x <- getWord8
  return $ chr (fromIntegral x)

-- | Generic .class file format
data ClassFile = ClassFile {
  magic :: Word32,                   -- ^ Magic value: 0xCAFEBABE
  minorVersion :: Word16,
  majorVersion :: Word16,
  constsPoolSize :: Word16,          -- ^ Number of items in constants pool
  constsPool :: [CpInfo],            -- ^ Constants pool itself
  accessFlags :: Word16,             -- ^ See @JVM.Types.AccessFlag@
  thisClass :: Word16,               -- ^ Constants pool item index for this class
  superClass :: Word16,              -- ^ --/-- for super class, zero for java.lang.Object
  interfacesCount :: Word16,         -- ^ Number of implemented interfaces
  interfaces :: [Word16],            -- ^ Constants pool item indexes for implemented interfaces
  classFieldsCount :: Word16,        -- ^ Number of class fileds
  classFields :: [FieldInfo],        -- ^ Class fields
  classMethodsCount :: Word16,       -- ^ Number of class methods
  classMethods :: [MethodInfo],      -- ^ Class methods
  classAttributesCount :: Word16,    -- ^ Number of class attributes
  classAttributes :: [AttributeInfo] -- ^ Class attributes
  }
  deriving (Eq, Show)

instance Binary ClassFile where
  put (ClassFile {..}) = do
    put magic
    put minorVersion
    put majorVersion
    put constsPoolSize
    forM_ constsPool put
    put accessFlags
    put thisClass
    put superClass
    put interfacesCount
    forM_ interfaces put
    put classFieldsCount
    forM_ classFields put
    put classMethodsCount
    forM_ classMethods put
    put classAttributesCount
    forM_ classAttributes put

  get = do
    magic <- get
    minor <- get
    major <- get
    poolsize <- get
    pool <- replicateM (fromIntegral poolsize - 1) get
    af <- get
    this <- get
    super <- get
    interfacesCount <- get
    ifaces <- replicateM (fromIntegral interfacesCount) get
    classFieldsCount <- get
    classFields <- replicateM (fromIntegral classFieldsCount) get
    classMethodsCount <- get
    classMethods <- replicateM (fromIntegral classMethodsCount) get
    asCount <- get
    as <- replicateM (fromIntegral $ asCount - 1) get
    return $ ClassFile magic minor major poolsize pool af this super
               interfacesCount ifaces classFieldsCount classFields classMethodsCount classMethods asCount as

-- | Field signature format
data FieldType =
    SignedByte -- ^ B
  | CharByte   -- ^ C
  | DoubleType -- ^ D
  | FloatType  -- ^ F
  | IntType    -- ^ I
  | LongInt    -- ^ J
  | ShortInt   -- ^ S
  | BoolType   -- ^ Z
  | ObjectType String -- ^ L <class name>
  | Array (Maybe Int) FieldType -- ^ [<type>
  deriving (Eq)

instance Show FieldType where
  show SignedByte = "byte"
  show CharByte = "char"
  show DoubleType = "double"
  show FloatType = "float"
  show IntType = "int"
  show LongInt = "long"
  show ShortInt = "short"
  show BoolType = "bool"
  show (ObjectType s) = "Object " ++ s
  show (Array Nothing t) = show t ++ "[]"
  show (Array (Just n) t) = show t ++ "[" ++ show n ++ "]"

-- | Class field signature
type FieldSignature = FieldType

-- | Try to read integer value from decimal representation
getInt :: Get (Maybe Int)
getInt = do
    s <- getDigits
    if null s
      then return Nothing
      else return $ Just (read s)
  where
    getDigits :: Get [Char]
    getDigits = do
      c <- lookAhead getChar8
      if isDigit c
        then do
             skip 1
             next <- getDigits
             return (c: next)
        else return []

instance Binary FieldType where
  put SignedByte = put 'B'
  put CharByte   = put 'C'
  put DoubleType = put 'D'
  put FloatType  = put 'F'
  put IntType    = put 'I'
  put LongInt    = put 'J'
  put ShortInt   = put 'S'
  put BoolType   = put 'Z'
  put (ObjectType name) = put 'L' >> put name
  put (Array Nothing sig) = put '[' >> put sig
  put (Array (Just n) sig) = put '[' >> put (show n) >> put sig

  get = do
    b <- getChar8
    case b of
      'B' -> return SignedByte
      'C' -> return CharByte
      'D' -> return DoubleType
      'F' -> return FloatType
      'I' -> return IntType
      'J' -> return LongInt
      'S' -> return ShortInt
      'Z' -> return BoolType
      'L' -> do
             name <- getToSemicolon
             return (ObjectType name)
      '[' -> do
             mbSize <- getInt
             sig <- get
             return (Array mbSize sig)
      _   -> fail $ "Unknown signature opening symbol: " ++ [b]

-- | Read string up to `;'
getToSemicolon :: Get String
getToSemicolon = do
  x <- get
  if x == ';'
    then return []
    else do
         next <- getToSemicolon
         return (x: next)

-- | Return value signature
data ReturnSignature =
    Returns FieldType
  | ReturnsVoid
  deriving (Eq)

instance Show ReturnSignature where
  show (Returns t) = show t
  show ReturnsVoid = "Void"

instance Binary ReturnSignature where
  put (Returns sig) = put sig
  put ReturnsVoid   = put 'V'

  get = do
    x <- lookAhead getChar8
    case x of
      'V' -> skip 1 >> return ReturnsVoid
      _   -> Returns <$> get

-- | Method argument signature
type ArgumentSignature = FieldType

-- | Class method argument signature
data MethodSignature =
    MethodSignature [ArgumentSignature] ReturnSignature
  deriving (Eq)

instance Show MethodSignature where
  show (MethodSignature args ret) = "(" ++ intercalate ", " (map show args) ++ ") returns " ++ show ret

instance Binary MethodSignature where
  put (MethodSignature args ret) = do
    put '('
    forM_ args put
    put ')'
    put ret

  get =  do
    x <- getChar8
    when (x /= '(') $
      fail "Cannot parse method signature: no starting `(' !"
    args <- getArgs
    y <- getChar8
    when (y /= ')') $
      fail "Internal error: method signature without `)' !?"
    ret <- get
    return (MethodSignature args ret)

-- | Read arguments signatures (up to `)')
getArgs :: Get [ArgumentSignature]
getArgs = whileJust getArg
  where
    getArg :: Get (Maybe ArgumentSignature)
    getArg = do
      x <- lookAhead getChar8
      if x == ')'
        then return Nothing
        else Just <$> get

whileJust :: (Monad m) => m (Maybe a) -> m [a]
whileJust m = do
  r <- m
  case r of
    Just x -> do
              next <- whileJust m
              return (x: next)
    Nothing -> return []

-- | Constant pool item format
data CpInfo =
    CONSTANT_Class {nameIndex :: Word16}                                          -- ^ 7
  | CONSTANT_Fieldref {classIndex :: Word16, nameAndTypeIndex :: Word16}	        -- ^ 9
  | CONSTANT_Methodref 	{classIndex :: Word16, nameAndTypeIndex :: Word16}        -- ^ 10
  | CONSTANT_InterfaceMethodref {classIndex :: Word16, nameAndTypeIndex :: Word16}-- ^ 11
  | CONSTANT_String {stringIndex :: Word16}                                       -- ^ 8
  | CONSTANT_Integer {fourBytes :: Word32}	                                      -- ^ 3
  | CONSTANT_Float Float                                                          -- ^ 4
  | CONSTANT_Long Word64                                                          -- ^ 5
  | CONSTANT_Double Double                                                        -- ^ 6
  | CONSTANT_NameAndType {nameIndex :: Word16, signatureIndex :: Word16} 	        -- ^ 12
  | CONSTANT_Utf8 {stringLength :: Word16, stringBytes :: B.ByteString}   	      -- ^ 1
  | CONSTANT_Unicode {stringLength :: Word16, stringBytes :: B.ByteString}   	    -- ^ 2
  deriving (Eq, Show)

instance Binary CpInfo where
  put (CONSTANT_Class i) = putWord8 7 >> put i
  put (CONSTANT_Fieldref i j) = putWord8 9 >> put i >> put j
  put (CONSTANT_Methodref i j) = putWord8 10 >> put i >> put j
  put (CONSTANT_InterfaceMethodref i j) = putWord8 11 >> put i >> put j
  put (CONSTANT_String i) = putWord8 8 >> put i
  put (CONSTANT_Integer x) = putWord8 3 >> put x
  put (CONSTANT_Float x)   = putWord8 4 >> putFloat32be x
  put (CONSTANT_Long x)    = putWord8 5 >> put x
  put (CONSTANT_Double x)  = putWord8 6 >> putFloat64be x
  put (CONSTANT_NameAndType i j) = putWord8 12 >> put i >> put j
  put (CONSTANT_Utf8 l bs) = putWord8 1 >> put l >> putLazyByteString bs
  put (CONSTANT_Unicode l bs) = putWord8 2 >> put l >> putLazyByteString bs

  get = do
    !offset <- bytesRead
    tag <- getWord8
    case tag of
      1 -> do
        l <- get
        bs <- getLazyByteString (fromIntegral l)
        return $ CONSTANT_Utf8 l bs
      2 -> do
        l <- get
        bs <- getLazyByteString (fromIntegral l)
        return $ CONSTANT_Unicode l bs
      3  -> CONSTANT_Integer   <$> get
      4  -> CONSTANT_Float     <$> getFloat32be
      5  -> CONSTANT_Long      <$> get
      6  -> CONSTANT_Double    <$> getFloat64be
      7  -> CONSTANT_Class     <$> get
      8  -> CONSTANT_String    <$> get
      9  -> CONSTANT_Fieldref  <$> get <*> get
      10 -> CONSTANT_Methodref <$> get <*> get
      11 -> CONSTANT_InterfaceMethodref <$> get <*> get
      12 -> CONSTANT_NameAndType <$> get <*> get
      _  -> fail $ "Unknown constants pool entry tag: " ++ show tag

-- | Class field format
data FieldInfo = FieldInfo {
  fieldAccessFlags :: Word16,
  fieldNameIndex :: Word16,
  fieldSignatureIndex :: Word16,
  fieldAttributesCount :: Word16,
  fieldAttributes :: [AttributeInfo] }
  deriving (Eq, Show)

instance Binary FieldInfo where
  put (FieldInfo {..}) = do
    put fieldAccessFlags 
    put fieldNameIndex
    put fieldSignatureIndex
    put fieldAttributesCount
    forM_ fieldAttributes put

  get = do
    af <- get
    ni <- get
    si <- get
    n <- get
    as <- replicateM (fromIntegral n) get
    return $ FieldInfo af ni si n as

-- | Class method format
data MethodInfo = MethodInfo {
  methodAccessFlags :: Word16,
  methodNameIndex :: Word16,
  methodSignatureIndex :: Word16,
  methodAttributesCount :: Word16,
  methodAttributes :: [AttributeInfo] }
  deriving (Eq, Show)

instance Binary MethodInfo where
  put (MethodInfo {..}) = do
    put methodAccessFlags
    put methodNameIndex 
    put methodSignatureIndex
    put methodAttributesCount 
    forM_ methodAttributes put

  get = do
    offset <- bytesRead
    af <- get
    ni <- get
    si <- get
    n <- get
    as <- replicateM (fromIntegral n) get
    return $ MethodInfo af ni si n as

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data AttributeInfo = AttributeInfo {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: B.ByteString }
  deriving (Eq, Show)

instance Binary AttributeInfo where
  put (AttributeInfo {..}) = do
    put attributeName
    putWord32be attributeLength
    putLazyByteString attributeValue

  get = do
    offset <- bytesRead
    name <- get
    len <- getWord32be
    value <- getLazyByteString (fromIntegral len)
    return $ AttributeInfo name len value


