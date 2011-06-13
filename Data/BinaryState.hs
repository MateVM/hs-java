{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.BinaryState where

import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as B
import Foreign.Storable
import Data.Word
import Data.Int

type PutState s a = State.StateT s Put.PutM a
type GetState s a = State.StateT s Binary.Get a

class BinaryState s a where
  put :: a -> PutState s ()
  get :: GetState s a

instance (Binary.Binary a) => BinaryState () a where
  put x = putZ x
  get = getZ

putZ :: (Binary.Binary a) => a -> PutState s ()
putZ x = State.lift (Binary.put x)

getZ :: (Binary.Binary a) => GetState s a
getZ = State.lift Binary.get

------------------------------------------------

encodeS :: (BinaryState s a) => s -> a -> B.ByteString
encodeS s a = Put.runPut $ State.evalStateT (put a) s

decodeS :: (BinaryState s a) => s -> B.ByteString -> a
decodeS s str = Get.runGet (State.evalStateT get s) str

encodeFile :: BinaryState s a => FilePath -> s -> a -> IO ()
encodeFile f s v = B.writeFile f (encodeS s v)

decodeFile :: BinaryState s a => FilePath -> s -> IO a
decodeFile f s = liftM (decodeS s) (B.readFile f)

------------------------------------------------

getByte :: GetState s Word8
getByte = State.lift Binary.getWord8

liftOffset :: (Binary.Binary a) => Integer -> (a -> Binary.Put) -> a -> PutState Integer ()
liftOffset d fn x = State.modify (+d) >> State.lift (fn x)

putByte :: Word8 -> PutState Integer ()
putByte x = liftOffset 1 Put.putWord8 x

isEmpty :: GetState s Bool
isEmpty = State.lift Get.isEmpty

skip :: Int -> GetState s ()
skip n = State.lift (Get.skip n)

getOffset :: PutState Integer Integer
getOffset = State.get

bytesRead :: GetState s Int64
bytesRead = State.lift Get.bytesRead

--------------------------------------------------

instance BinaryState Integer Word8 where
  put x = putByte x
  get = getZ

instance BinaryState Integer Word16 where
  put x = liftOffset 2 Binary.put x
  get = getZ

instance BinaryState Integer Word32 where
  put x = liftOffset 4 Binary.put x
  get = getZ

instance (BinaryState s a, BinaryState s b) => BinaryState s (a,b) where
  put (x,y) = put x >> put y
  get = do
    x <- get
    y <- get
    return (x,y)

--------------------------------------------------

-- instance (Binary.Binary a, Storable a) => BinaryState Integer a where
--   put x = liftOffset (fromIntegral $ sizeOf x) Binary.put x
--   get = getZ
