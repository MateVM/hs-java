{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}
-- | This module defines Generate monad, which helps generating JVM code and
-- creating Java class constants pool.
module JVM.Builder.Monad
  (GState (..),
   emptyGState,
   Generate,
   addToPool,
   i0, i1, i8,
   newMethod,
   setStackSize, setMaxLocals,
   generate
  ) where

import Control.Monad.State as St
import Data.Word
import Data.List
import Data.Binary
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B

import JVM.Common ()  -- import instances only
import JVM.ClassFile
import JVM.Assembler

-- | Generator state
data GState = GState {
  generated :: [Instruction],               -- ^ Already generated code (in current method)
  currentPool :: Pool Direct,             -- ^ Already generated constants pool
  doneMethods :: [Method Direct],         -- ^ Already generated class methods
  currentMethod :: Maybe (Method Direct), -- ^ Current method
  stackSize :: Word16,                      -- ^ Maximum stack size for current method
  locals :: Word16                          -- ^ Maximum number of local variables for current method
  }
  deriving (Eq,Show)

-- | Empty generator state
emptyGState ::  GState
emptyGState = GState {
  generated = [],
  currentPool = M.empty,
  doneMethods = [],
  currentMethod = Nothing,
  stackSize = 496,
  locals = 0 }

-- | Generate monad
type Generate a = State GState a

-- | Append a constant to pool
appendPool :: Constant Direct -> Pool Direct -> (Pool Direct, Word16)
appendPool c pool =
  let size = fromIntegral (M.size pool)
      pool' = M.insert size c pool
  in  (pool', size)

-- | Add a constant to pool
addItem :: Constant Direct -> Generate Word16
addItem c = do
  pool <- St.gets currentPool
  case lookupPool c pool of
    Just i -> return (i+1)
    Nothing -> do
      let (pool', i) = appendPool c pool
      st <- St.get
      St.put $ st {currentPool = pool'}
      return (i+1)

-- | Lookup in a pool
lookupPool :: Constant Direct -> Pool Direct -> Maybe Word16
lookupPool c pool =
  fromIntegral `fmap` findIndex (== c) (M.elems pool)

addNT :: Binary (Signature a) => NameType a -> Generate Word16
addNT (NameType name sig) = do
  let bsig = encode sig
  x <- addItem (CNameType name bsig)
  addItem (CUTF8 name)
  addItem (CUTF8 bsig)
  return x

addSig :: MethodSignature -> Generate Word16
addSig c@(MethodSignature args ret) = do
  let bsig = encode c
  addItem (CUTF8 bsig)

-- | Add a constant into pool
addToPool :: Constant Direct -> Generate Word16
addToPool c@(CClass str) = do
  addItem (CUTF8 str)
  addItem c
addToPool c@(CField cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CMethod cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CIfaceMethod cls name) = do
  addToPool (CClass cls)
  addNT name
  addItem c
addToPool c@(CString str) = do
  addToPool (CUTF8 str)
  addItem c
addToPool c@(CNameType name sig) = do
  addItem (CUTF8 name)
  addItem (CUTF8 sig)
  addItem c
addToPool c = addItem c

putInstruction :: Instruction -> Generate ()
putInstruction instr = do
  st <- St.get
  let code = generated st
  St.put $ st {generated = code ++ [instr]}

-- | Generate one (zero-arguments) instruction
i0 :: Instruction -> Generate ()
i0 = putInstruction

-- | Generate one one-argument instruction
i1 :: (Word16 -> Instruction) -> Constant Direct -> Generate ()
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

-- | Generate one one-argument instruction
i8 :: (Word8 -> Instruction) -> Constant Direct -> Generate ()
i8 fn c = do
  ix <- addToPool c
  i0 (fn $ fromIntegral ix)

-- | Set maximum stack size for current method
setStackSize :: Word16 -> Generate ()
setStackSize n = do
  st <- St.get
  St.put $ st {stackSize = n}

-- | Set maximum number of local variables for current method
setMaxLocals :: Word16 -> Generate ()
setMaxLocals n = do
  st <- St.get
  St.put $ st {locals = n}

-- | Start generating new method
startMethod :: [AccessFlag] -> B.ByteString -> MethodSignature -> Generate ()
startMethod flags name sig = do
  addToPool (CString name)
  addSig sig
  setStackSize 4096
  setMaxLocals 100
  st <- St.get
  let method = Method {
    methodAccessFlags = S.fromList flags,
    methodName = name,
    methodSignature = sig,
    methodAttributesCount = 0,
    methodAttributes = AR M.empty }
  St.put $ st {generated = [],
               currentMethod = Just method }

-- | End of method generation
endMethod :: Generate ()
endMethod = do
  m <- St.gets currentMethod
  code <- St.gets genCode
  case m of
    Nothing -> fail "endMethod without startMethod!"
    Just method -> do
      let method' = method {methodAttributes = AR $ M.fromList [("Code", encodeMethod code)],
                            methodAttributesCount = 1}
      st <- St.get
      St.put $ st {generated = [],
                   currentMethod = Nothing,
                   doneMethods = doneMethods st ++ [method']}

-- | Generate new method
newMethod :: [AccessFlag]               -- ^ Access flags for method (public, static etc)
          -> B.ByteString               -- ^ Method name
          -> [ArgumentSignature]        -- ^ Signatures of method arguments
          -> ReturnSignature            -- ^ Method return signature
          -> Generate ()                -- ^ Generator for method code
          -> Generate (NameType Method)
newMethod flags name args ret gen = do
  let sig = MethodSignature args ret
  startMethod flags name sig
  gen
  endMethod
  return (NameType name sig)

-- | Convert Generator state to method Code.
genCode :: GState -> Code
genCode st = Code {
    codeStackSize = stackSize st,
    codeMaxLocals = locals st,
    codeLength = len,
    codeInstructions = generated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = AP [] }
  where
    len = fromIntegral $ B.length $ encodeInstructions (generated st)

-- | Start class generation.
initClass :: B.ByteString -> Generate Word16
initClass name = do
  addToPool (CClass "java/lang/Object")
  addToPool (CClass name)
  addToPool (CString "Code")

-- | Generate a class
generate :: B.ByteString -> Generate () -> Class Direct
generate name gen =
  let generator = do
        initClass name
        gen
      res = execState generator emptyGState
      code = genCode res
  in  Class {
        magic = 0xCAFEBABE,
        minorVersion = 0,
        majorVersion = 50,
        constsPoolSize = fromIntegral $ M.size (currentPool res),
        constsPool = currentPool res,
        accessFlags = S.fromList [ACC_PUBLIC, ACC_STATIC],
        thisClass = name,
        superClass = "java/lang/Object",
        interfacesCount = 0,
        interfaces = [],
        classFieldsCount = 0,
        classFields = [],
        classMethodsCount = fromIntegral $ length (doneMethods res),
        classMethods = doneMethods res,
        classAttributesCount = 0,
        classAttributes = AR M.empty }

