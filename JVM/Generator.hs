{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}
module JVM.Generator where

import Control.Monad.State as St
import Data.Array
import Data.Word
import Data.List
import Data.Binary
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B

import JVM.Types
import JVM.ClassFile
import JVM.Assembler

data GState = GState {
  generated :: [Instruction],
  currentPool :: Pool,
  doneMethods :: [Method],
  currentMethod :: Maybe Method}
  deriving (Eq,Show)

emptyGState = GState {
  generated = [],
  currentPool = listArray (0,0) [CInteger 0],
  doneMethods = [],
  currentMethod = Nothing }

type Generate a = State GState a

appendPool :: Constant -> Pool -> (Pool, Word16)
appendPool c pool =
  let list = assocs pool
      size = fromIntegral (length list)
      list' = list ++ [(size, c)]
  in  (array (0, size) list',
       size)

addItem :: Constant -> Generate Word16
addItem c = do
  pool <- St.gets currentPool
  if pool ! 0 == CInteger 0
    then do
         st <- St.get
         St.put $ st {currentPool = listArray (0,0) [c]}
         return 1
    else case lookupPool c pool of
          Just i -> return i
          Nothing -> do
            let (pool', i) = appendPool c pool
            st <- St.get
            St.put $ st {currentPool = pool'}
            return (i+1)

lookupPool :: Constant -> Pool -> Maybe Word16
lookupPool c pool =
  fromIntegral `fmap` findIndex (== c) (elems pool)

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

addToPool :: Constant -> Generate Word16
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

i1 :: (Word16 -> Instruction) -> Constant -> Generate ()
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

i8 :: (Word8 -> Instruction) -> Constant -> Generate ()
i8 fn c = do
  ix <- addToPool c
  i0 (fn $ fromIntegral ix)

startMethod :: [AccessFlag] -> B.ByteString -> MethodSignature -> Generate ()
startMethod flags name sig = do
  addToPool (CString name)
  addSig sig
  st <- St.get
  let method = Method {
    methodAccess = S.fromList flags,
    methodName = name,
    methodSignature = sig,
    methodAttrs = M.empty }
  St.put $ st {generated = [],
               currentMethod = Just method }

endMethod :: Generate ()
endMethod = do
  m <- St.gets currentMethod
  code <- St.gets genCode
  case m of
    Nothing -> fail "endMethod without startMethod!"
    Just method -> do
      let method' = method {methodAttrs = M.fromList [("Code", encodeMethod code)] }
      st <- St.get
      St.put $ st {generated = [],
                   currentMethod = Nothing,
                   doneMethods = doneMethods st ++ [method']}

newMethod :: [AccessFlag] -> B.ByteString -> [ArgumentSignature] -> ReturnSignature -> Generate () -> Generate ()
newMethod flags name args ret gen = do
  startMethod flags name (MethodSignature args ret)
  gen
  endMethod

genCode :: GState -> Code
genCode st = Code {
    codeStackSize = 4096,
    codeMaxLocals = 100,
    codeLength = len,
    codeInstructions = generated st,
    codeExceptionsN = 0,
    codeExceptions = [],
    codeAttrsN = 0,
    codeAttributes = [] }
  where
    len = fromIntegral $ B.length $ encodeInstructions (generated st)

initClass :: B.ByteString -> Generate Word16
initClass name = do
  addToPool (CClass "java/lang/Object")
  addToPool (CClass name)
  addToPool (CString "Code")

generate :: B.ByteString -> Generate () -> Class
generate name gen =
  let generator = do
        initClass name
        gen
      res = execState generator emptyGState
      code = genCode res
  in  Class {
        constantPool = currentPool res,
        classAccess = S.fromList [ACC_PUBLIC, ACC_STATIC],
        this = name,
        super = Just "java/lang/Object",
        implements = [],
        fields = [],
        methods = doneMethods res,
        classAttrs = M.empty }

