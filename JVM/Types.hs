{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- | This module declares `high-level' data types for Java classes, methods etc.
module JVM.Types where

import Codec.Binary.UTF8.String hiding (encode, decode)
import Control.Applicative
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.String
import qualified Data.Set as S
import qualified Data.Map as M

import JVM.ClassFile

instance IsString B.ByteString where
  fromString s = B.pack $ map (fromIntegral . ord) $ encodeString s

toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr

toCharList :: B.ByteString -> [Int]
toCharList bstr = map fromIntegral $ B.unpack bstr

-- | Constant pool
type Pool = M.Map Word16 Constant

poolSize :: Pool -> Int
poolSize = M.size

(!) :: (Ord k) => M.Map k a -> k -> a
(!) = (M.!)

showListIx :: (Show a) => [a] -> String
showListIx list = unlines $ zipWith s [1..] list
  where s i x = show i ++ ":\t" ++ show x

class HasAttributes a where
  attributes :: a -> Attributes

-- | Java class
data Class = Class {
  constantPool :: Pool,
  classAccess :: Access,
  this :: B.ByteString,        -- ^ this class name
  super :: Maybe B.ByteString, -- ^ super class name
  implements :: [B.ByteString], -- ^ implemented interfaces
  fields :: [Field],
  methods :: [Method],
  classAttrs :: Attributes
  }
  deriving (Eq, Show)

instance HasAttributes Class where
  attributes = classAttrs

class HasSignature a where
  type Signature a

-- | Name and signature pair. Used for methods and fields.
data NameType a = NameType {
  ntName :: B.ByteString,
  ntSignature :: Signature a }

instance Show (Signature a) => Show (NameType a) where
  show (NameType n t) = toString n ++ ": " ++ show t

deriving instance Eq (Signature a) => Eq (NameType a)

-- | Constant pool item
data Constant =
    CClass B.ByteString
  | CField {refClass :: B.ByteString, fieldNameType :: NameType Field}
  | CMethod {refClass :: B.ByteString, nameType :: NameType Method}
  | CIfaceMethod {refClass :: B.ByteString, nameType :: NameType Method}
  | CString B.ByteString
  | CInteger Word32
  | CFloat Float
  | CLong Integer
  | CDouble Double
  | CNameType B.ByteString B.ByteString
  | CUTF8 {getString :: B.ByteString}
  | CUnicode {getString :: B.ByteString}
  deriving (Eq)

className ::  Constant -> B.ByteString
className (CClass s) = s
className x = error $ "Not a class: " ++ show x

instance Show Constant where
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

-- | Class field
data Field = Field {
  fieldAccess :: Access,
  fieldName :: B.ByteString,
  fieldSignature :: FieldSignature,
  fieldAttrs :: Attributes }
  deriving (Eq, Show)

instance HasSignature Field where
  type Signature Field = FieldSignature

instance HasAttributes Field where
  attributes = fieldAttrs

-- | Class method
data Method = Method {
  methodAccess :: Access,
  methodName :: B.ByteString,
  methodSignature :: MethodSignature,
  methodAttrs :: Attributes }
  deriving (Eq, Show)

instance HasSignature Method where
  type Signature Method = MethodSignature

instance HasAttributes Method where
  attributes = methodAttrs

-- | Set of access flags
type Access = S.Set AccessFlag

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

-- | Generic attribute
data Attribute = Attribute {
  attrName :: B.ByteString,
  attrValue :: B.ByteString }
  deriving (Eq, Show)

-- | Set of attributes
type Attributes = M.Map B.ByteString B.ByteString

instance (Binary (Signature a)) => Binary (NameType a) where
  put (NameType n t) = putLazyByteString n >> put t

  get = NameType <$> get <*> get

byteString ::  (Binary t) => t -> B.ByteString
byteString x = runPut (put x)

