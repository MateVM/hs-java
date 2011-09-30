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

toCharList :: B.ByteString -> [Int]
toCharList bstr = map fromIntegral $ B.unpack bstr

poolSize :: Pool stage -> Int
poolSize = M.size

(!) :: (Ord k) => M.Map k a -> k -> a
(!) = (M.!)

showListIx :: (Show a) => [a] -> String
showListIx list = unlines $ zipWith s [1..] list
  where s i x = show i ++ ":\t" ++ show x

byteString ::  (Binary t) => t -> B.ByteString
byteString x = runPut (put x)

