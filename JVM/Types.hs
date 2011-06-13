{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module JVM.Types where

import Codec.Binary.UTF8.String hiding (encode, decode)
import Data.Array
import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Char
import Data.String
import qualified Data.Set as S
import qualified Data.Map as M

import JVM.ClassFile

instance IsString B.ByteString where
  fromString s = B.pack $ map (fromIntegral . ord) $ encodeString s

toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr

type Pool = Array Word16 Constant

class HasAttributes a where
  attributes :: a -> Attributes

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

data NameType a = NameType {
  ntName :: B.ByteString,
  ntSignature :: Signature a }

instance Show (Signature a) => Show (NameType a) where
  show (NameType n t) = toString n ++ ": " ++ show t

deriving instance Eq (Signature a) => Eq (NameType a)

data Constant =
    CClass {className :: B.ByteString}
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

type Access = S.Set AccessFlag

data AccessFlag =
    ACC_PUBLIC 	     -- 0x0001 Видимый для всех 	Класс, Метод, Переменная
  | ACC_PRIVATE 	   -- 0x0002 Видимый только для определяемого класса 	Метод, Переменная
  | ACC_PROTECTED 	 -- 0x0004 Видимый для подклассов 	Метод, Переменная
  | ACC_STATIC 	     -- 0x0008 Переменная или метод статические 	Метод, Переменная
  | ACC_FINAL 	     -- 0x0010 Нет дальнейшей подкласификации, обхода или присваивания после инициализации 	Класс, Метод, Переменная
  | ACC_SYNCHRONIZED -- 0x0020 Использует возврат в блокировке монитора 	Метод
  | ACC_VOLATILE 	   -- 0x0040 Не может помещать в кеш 	Переменная
  | ACC_TRANSIENT 	 -- 0x0080 Не может боть написан или прочитан постоянным объектом управления 	Перемення
  | ACC_NATIVE 	     -- 0x0100 Реализован в других языках 	Метод
  | ACC_INTERFACE 	 -- 0x0200 интерфейс 	Класс
  | ACC_ABSTRACT 	   -- 0x0400 Ничего не предусматривает 	Класс, Метод
  deriving (Eq, Show, Ord)

data Attribute = Attribute {
  attrName :: B.ByteString,
  attrValue :: B.ByteString }
  deriving (Eq, Show)

class AttributeValue a where
  decodeAttribute :: B.ByteString -> a
  encodeAttribute :: a -> B.ByteString

type Attributes = M.Map B.ByteString B.ByteString

