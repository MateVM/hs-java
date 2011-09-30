{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
-- | This module declares (low-level) data types for Java .class files
-- structures, and Binary instances to read/write them.
module JVM.ClassFile
  (Attribute (..),
   FieldType (..),
   FieldSignature, MethodSignature (..), ReturnSignature (..),
   ArgumentSignature (..),
   Pool, Link,
   Method (..), Field (..), Class (..),
   Constant (..),
   Pointers, Resolved,
   NameType (..),
   HasSignature (..), HasAttributes (..),
   AccessFlag (..), AccessFlags,
   Attributes (..),
   className
  )
  where

import Control.Monad
import Control.Applicative
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String hiding (encode, decode)

-- | Read one-byte Char
getChar8 :: Get Char
getChar8 = do
  x <- getWord8
  return $ chr (fromIntegral x)

toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr

type family Link s a

data Pointers = Pointers

data Resolved = Resolved

type instance Link Pointers a = Word16

type instance Link Resolved a = a

type family AccessFlags stage

type instance AccessFlags Pointers = Word16

type instance AccessFlags Resolved = S.Set AccessFlag

type family Attributes stage

type instance Attributes Pointers = [Attribute]
type instance Attributes Resolved = M.Map B.ByteString B.ByteString

-- | Access flags. Used for classess, methods, variables.
data AccessFlag =
    ACC_PUBLIC 	     -- ^ 0x0001 Visible for all
  | ACC_PRIVATE 	   -- ^ 0x0002 Visible only for defined class
  | ACC_PROTECTED 	 -- ^ 0x0004 Visible only for subclasses
  | ACC_STATIC 	     -- ^ 0x0008 Static method or variable
  | ACC_FINAL 	     -- ^ 0x0010 No further subclassing or assignments
  | ACC_SYNCHRONIZED -- ^ 0x0020 Uses monitors
  | ACC_VOLATILE 	   -- ^ 0x0040 Could not be cached
  | ACC_TRANSIENT 	 -- ^ 0x0080 
  | ACC_NATIVE 	     -- ^ 0x0100 Implemented in other language
  | ACC_INTERFACE 	 -- ^ 0x0200 Class is interface
  | ACC_ABSTRACT 	   -- ^ 0x0400 
  deriving (Eq, Show, Ord, Enum)

class HasSignature a where
  type Signature a

instance HasSignature Field where
  type Signature Field = FieldSignature

instance HasSignature Method where
  type Signature Method = MethodSignature

-- | Name and signature pair. Used for methods and fields.
data NameType a = NameType {
  ntName :: B.ByteString,
  ntSignature :: Signature a }

instance Show (Signature a) => Show (NameType a) where
  show (NameType n t) = toString n ++ ": " ++ show t

deriving instance Eq (Signature a) => Eq (NameType a)

instance (Binary (Signature a)) => Binary (NameType a) where
  put (NameType n t) = putLazyByteString n >> put t

  get = NameType <$> get <*> get

-- | Constant pool item
data Constant stage =
    CClass B.ByteString
  | CField {refClass :: Link stage B.ByteString, fieldNameType :: Link stage (NameType Field)}
  | CMethod {refClass :: Link stage B.ByteString, nameType :: Link stage (NameType Method)}
  | CIfaceMethod {refClass :: Link stage B.ByteString, nameType :: Link stage (NameType Method)}
  | CString (Link stage B.ByteString)
  | CInteger Word32
  | CFloat Float
  | CLong Integer
  | CDouble Double
  | CNameType (Link stage B.ByteString) (Link stage B.ByteString)
  | CUTF8 {getString :: B.ByteString}
  | CUnicode {getString :: B.ByteString}

className ::  Constant Resolved -> B.ByteString
className (CClass s) = s
className x = error $ "Not a class: " ++ show x

instance Show (Constant Resolved) where
  show (CClass name) = "class " ++ toString name
  show (CField cls nt) = "field " ++ toString cls ++ "." ++ show nt
  show (CMethod cls nt) = "method " ++ toString cls ++ "." ++ show nt
  show (CIfaceMethod cls nt) = "interface method " ++ toString cls ++ "." ++ show nt
  show (CString s) = "String \"" ++ toString s ++ "\""
  show (CInteger x) = show x
  show (CFloat x) = show x
  show (CLong x) = show x
  show (CDouble x) = show x
  show (CNameType name tp) = toString name ++ ": " ++ toString tp
  show (CUTF8 s) = "UTF8 \"" ++ toString s ++ "\""
  show (CUnicode s) = "Unicode \"" ++ toString s ++ "\""

-- | Constant pool
type Pool stage = M.Map Word16 (Constant stage)

-- | Generic .class file format
data Class stage = Class {
  magic :: Word32,                   -- ^ Magic value: 0xCAFEBABE
  minorVersion :: Word16,
  majorVersion :: Word16,
  constsPoolSize :: Word16,          -- ^ Number of items in constants pool
  constsPool :: Pool stage,            -- ^ Constants pool itself
  accessFlags :: AccessFlags stage,             -- ^ See @JVM.Types.AccessFlag@
  thisClass :: Link stage B.ByteString,               -- ^ Constants pool item index for this class
  superClass :: Link stage B.ByteString,              -- ^ --/-- for super class, zero for java.lang.Object
  interfacesCount :: Word16,         -- ^ Number of implemented interfaces
  interfaces :: [Link stage B.ByteString],            -- ^ Constants pool item indexes for implemented interfaces
  classFieldsCount :: Word16,        -- ^ Number of class fileds
  classFields :: [Field stage],        -- ^ Class fields
  classMethodsCount :: Word16,       -- ^ Number of class methods
  classMethods :: [Method stage],      -- ^ Class methods
  classAttributesCount :: Word16,    -- ^ Number of class attributes
  classAttributes :: Attributes stage -- ^ Class attributes
  }

deriving instance Eq (Constant Pointers)
deriving instance Eq (Constant Resolved)
deriving instance Show (Constant Pointers)

instance Binary (Class Pointers) where
  put (Class {..}) = do
    put magic
    put minorVersion
    put majorVersion
    put constsPoolSize
    forM_ (M.elems constsPool) put
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
    as <- replicateM (fromIntegral $ asCount) get
    let pool' = M.fromList $ zip [1..] pool
    return $ Class magic minor major poolsize pool' af this super
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
  | ObjectType String -- ^ L @{class name}@
  | Array (Maybe Int) FieldType -- ^ @[{type}@
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

putString :: String -> Put
putString str = forM_ str put

instance Binary FieldType where
  put SignedByte = put 'B'
  put CharByte   = put 'C'
  put DoubleType = put 'D'
  put FloatType  = put 'F'
  put IntType    = put 'I'
  put LongInt    = put 'J'
  put ShortInt   = put 'S'
  put BoolType   = put 'Z'
  put (ObjectType name) = put 'L' >> putString name >> put ';'
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

instance Binary (Constant Pointers) where
  put (CClass i) = putWord8 7 >> put i
  put (CField i j) = putWord8 9 >> put i >> put j
  put (CMethod i j) = putWord8 10 >> put i >> put j
  put (CIfaceMethod i j) = putWord8 11 >> put i >> put j
  put (CString i) = putWord8 8 >> put i
  put (CInteger x) = putWord8 3 >> put x
  put (CFloat x)   = putWord8 4 >> putFloat32be x
  put (CLong x)    = putWord8 5 >> put x
  put (CDouble x)  = putWord8 6 >> putFloat64be x
  put (CNameType i j) = putWord8 12 >> put i >> put j
  put (CUTF8 bs) = do
                   putWord8 1
                   put (fromIntegral (B.length bs) :: Word16)
                   putLazyByteString bs
  put (CUnicode bs) = do
                   putWord8 2
                   put (fromIntegral (B.length bs) :: Word16)
                   putLazyByteString bs

  get = do
    !offset <- bytesRead
    tag <- getWord8
    case tag of
      1 -> do
        l <- get
        bs <- getLazyByteString (fromIntegral (l :: Word16))
        return $ CUTF8 bs
      2 -> do
        l <- get
        bs <- getLazyByteString (fromIntegral (l :: Word16))
        return $ CUnicode bs
      3  -> CInteger   <$> get
      4  -> CFloat     <$> getFloat32be
      5  -> CLong      <$> get
      6  -> CDouble    <$> getFloat64be
      7  -> CClass     <$> get
      8  -> CString    <$> get
      9  -> CField     <$> get <*> get
      10 -> CMethod    <$> get <*> get
      11 -> CIfaceMethod <$> get <*> get
      12 -> CNameType    <$> get <*> get
      _  -> fail $ "Unknown constants pool entry tag: " ++ show tag

-- | Class field format
data Field stage = Field {
  fieldAccessFlags :: AccessFlags stage,
  fieldName :: Link stage B.ByteString,
  fieldSignature :: Link stage FieldSignature,
  fieldAttributesCount :: Word16,
  fieldAttributes :: Attributes stage }

deriving instance Eq (Field Pointers)
deriving instance Eq (Field Resolved)
deriving instance Show (Field Pointers)
deriving instance Show (Field Resolved)

instance Binary (Field Pointers) where
  put (Field {..}) = do
    put fieldAccessFlags 
    put fieldName
    put fieldSignature
    put fieldAttributesCount
    forM_ fieldAttributes put

  get = do
    af <- get
    ni <- get
    si <- get
    n <- get
    as <- replicateM (fromIntegral n) get
    return $ Field af ni si n as

-- | Class method format
data Method stage = Method {
  methodAccessFlags :: Attributes stage,
  methodName :: Link stage B.ByteString,
  methodSignature :: Link stage MethodSignature,
  methodAttributesCount :: Word16,
  methodAttributes :: Attributes stage }

deriving instance Eq (Method Pointers)
deriving instance Eq (Method Resolved)
deriving instance Show (Method Pointers)
deriving instance Show (Method Resolved)

instance Binary (Method Pointers) where
  put (Method {..}) = do
    put methodAccessFlags
    put methodName
    put methodSignature
    put methodAttributesCount 
    forM_ methodAttributes put

  get = do
    offset <- bytesRead
    af <- get
    ni <- get
    si <- get
    n <- get
    as <- replicateM (fromIntegral n) get
    return $ Method af ni si n as

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data Attribute = Attribute {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: B.ByteString }
  deriving (Eq, Show)

instance Binary Attribute where
  put (Attribute {..}) = do
    put attributeName
    putWord32be attributeLength
    putLazyByteString attributeValue

  get = do
    offset <- bytesRead
    name <- get
    len <- getWord32be
    value <- getLazyByteString (fromIntegral len)
    return $ Attribute name len value

class HasAttributes a where
  attributes :: a stage -> Attributes stage

instance HasAttributes Class where
  attributes = classAttributes

instance HasAttributes Field where
  attributes = fieldAttributes

instance HasAttributes Method where
  attributes = methodAttributes

