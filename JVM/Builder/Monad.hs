{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module JVM.Builder.Monad where

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

data GState = GState {
  generated :: [Instruction],
  currentPool :: Pool Resolved,
  doneMethods :: [Method Resolved],
  currentMethod :: Maybe (Method Resolved),
  stackSize :: Word16,
  locals :: Word16 }
  deriving (Eq,Show)

emptyGState = GState {
  generated = [],
  currentPool = M.empty,
  doneMethods = [],
  currentMethod = Nothing,
  stackSize = 496,
  locals = 0 }

type Generate a = State GState a

appendPool :: Constant Resolved -> Pool Resolved -> (Pool Resolved, Word16)
appendPool c pool =
  let size = fromIntegral (M.size pool)
      pool' = M.insert size c pool
  in  (pool', size)

addItem :: Constant Resolved -> Generate Word16
addItem c = do
  pool <- St.gets currentPool
  case lookupPool c pool of
    Just i -> return (i+1)
    Nothing -> do
      let (pool', i) = appendPool c pool
      st <- St.get
      St.put $ st {currentPool = pool'}
      return (i+1)

lookupPool :: Constant Resolved -> Pool Resolved -> Maybe Word16
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

addToPool :: Constant Resolved -> Generate Word16
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

i0 :: Instruction -> Generate ()
i0 = putInstruction

i1 :: (Word16 -> Instruction) -> Constant Resolved -> Generate ()
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

i8 :: (Word8 -> Instruction) -> Constant Resolved -> Generate ()
i8 fn c = do
  ix <- addToPool c
  i0 (fn $ fromIntegral ix)

setStackSize :: Word16 -> Generate ()
setStackSize n = do
  st <- St.get
  St.put $ st {stackSize = n}

setMaxLocals :: Word16 -> Generate ()
setMaxLocals n = do
  st <- St.get
  St.put $ st {locals = n}

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

newMethod :: [AccessFlag] -> B.ByteString -> [ArgumentSignature] -> ReturnSignature -> Generate () -> Generate (NameType Method)
newMethod flags name args ret gen = do
  let sig = MethodSignature args ret
  startMethod flags name sig
  gen
  endMethod
  return (NameType name sig)

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

initClass :: B.ByteString -> Generate Word16
initClass name = do
  addToPool (CClass "java/lang/Object")
  addToPool (CClass name)
  addToPool (CString "Code")

generate :: B.ByteString -> Generate () -> Class Resolved
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

