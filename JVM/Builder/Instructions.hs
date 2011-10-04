-- | This module exports shortcuts for some of JVM instructions (which are defined in JVM.Assembler).
-- These functions get Constants, put them into constants pool and generate instruction using index
-- of constant in the pool.
module JVM.Builder.Instructions where

import Data.Word
import qualified Data.ByteString.Lazy as B

import JVM.ClassFile
import JVM.Assembler
import JVM.Builder.Monad

nop :: Generator g => g ()
nop = i0 NOP
aconst_null :: Generator g => g ()
aconst_null = i0 ACONST_NULL
iconst_m1 :: Generator g => g ()
iconst_m1 = i0 ICONST_M1
iconst_0 :: Generator g => g ()
iconst_0 = i0 ICONST_0
iconst_1 :: Generator g => g ()
iconst_1 = i0 ICONST_1
iconst_2 :: Generator g => g ()
iconst_2 = i0 ICONST_2
iconst_3 :: Generator g => g ()
iconst_3 = i0 ICONST_3
iconst_4 :: Generator g => g ()
iconst_4 = i0 ICONST_4
iconst_5 :: Generator g => g ()
iconst_5 = i0 ICONST_5
lconst_0 :: Generator g => g ()
lconst_0 = i0 LCONST_0
lconst_1 :: Generator g => g ()
lconst_1 = i0 LCONST_1
fconst_0 :: Generator g => g ()
fconst_0 = i0 FCONST_0
fconst_1 :: Generator g => g ()
fconst_1 = i0 FCONST_1
fconst_2 :: Generator g => g ()
fconst_2 = i0 FCONST_2
dconst_0 :: Generator g => g ()
dconst_0 = i0 DCONST_0
dconst_1 :: Generator g => g ()
dconst_1 = i0 DCONST_1

bipush :: Generator g => Word8 -> g ()
bipush x = i0 (BIPUSH x)
sipush :: Generator g => Word16 -> g ()
sipush x = i0 (SIPUSH x)

ldc1 :: Generator g => Constant Direct -> g ()
ldc1 x = i8 LDC1 x
ldc2 :: Generator g => Constant Direct -> g ()
ldc2 x = i1 LDC2 x
ldc2w :: Generator g => Constant Direct -> g ()
ldc2w x = i1 LDC2W x
iload :: Generator g => Constant Direct -> g ()
iload x = i8 ILOAD x
lload :: Generator g => Constant Direct -> g ()
lload x = i8 LLOAD x
fload :: Generator g => Constant Direct -> g ()
fload x = i8 FLOAD x
dload :: Generator g => Constant Direct -> g ()
dload x = i8 DLOAD x
aload :: Generator g => Constant Direct -> g ()
aload x = i8 ALOAD x

iload_ :: Generator g => IMM -> g ()
iload_ x = i0 (ILOAD_ x)
lload_ :: Generator g => IMM -> g ()
lload_ x = i0 (LLOAD_ x)
fload_ :: Generator g => IMM -> g ()
fload_ x = i0 (FLOAD_ x)
dload_ :: Generator g => IMM -> g ()
dload_ x = i0 (DLOAD_ x)
aload_ :: Generator g => IMM -> g ()
aload_ x = i0 (ALOAD_ x)

iaload :: Generator g => g ()
iaload = i0 IALOAD
laload :: Generator g => g ()
laload = i0 LALOAD
faload :: Generator g => g ()
faload = i0 FALOAD
daload :: Generator g => g ()
daload = i0 DALOAD
aaload :: Generator g => g ()
aaload = i0 AALOAD
caload :: Generator g => g ()
caload = i0 CALOAD
saload :: Generator g => g ()
saload = i0 SALOAD

istore :: Generator g => Constant Direct -> g ()
istore x = i8 ISTORE x
lstore :: Generator g => Constant Direct -> g ()
lstore x = i8 LSTORE x
fstore :: Generator g => Constant Direct -> g ()
fstore x = i8 FSTORE x
dstore :: Generator g => Constant Direct -> g ()
dstore x = i8 DSTORE x
astore :: Generator g => Constant Direct -> g ()
astore x = i8 ASTORE x

istore_ :: Generator g => Word8 -> g ()
istore_ x = i0 (ISTORE x)
lstore_ :: Generator g => Word8 -> g ()
lstore_ x = i0 (LSTORE x)
fstore_ :: Generator g => Word8 -> g ()
fstore_ x = i0 (FSTORE x)
dstore_ :: Generator g => Word8 -> g ()
dstore_ x = i0 (DSTORE x)
astore_ :: Generator g => Word8 -> g ()
astore_ x = i0 (ASTORE x)

iastore :: Generator g => g ()
iastore = i0 IASTORE
lastore :: Generator g => g ()
lastore = i0 LASTORE
fastore :: Generator g => g ()
fastore = i0 FASTORE
dastore :: Generator g => g ()
dastore = i0 DASTORE
aastore :: Generator g => g ()
aastore = i0 AASTORE
bastore :: Generator g => g ()
bastore = i0 BASTORE
castore :: Generator g => g ()
castore = i0 CASTORE
sastore :: Generator g => g ()
sastore = i0 SASTORE

pop :: Generator g => g ()
pop     = i0 POP    
pop2 :: Generator g => g ()
pop2    = i0 POP2   
dup :: Generator g => g ()
dup     = i0 DUP    
dup_x1 :: Generator g => g ()
dup_x1  = i0 DUP_X1 
dup_x2 :: Generator g => g ()
dup_x2  = i0 DUP_X2 
dup2 :: Generator g => g ()
dup2    = i0 DUP2   
dup2_x1 :: Generator g => g ()
dup2_x1 = i0 DUP2_X1
dup2_x2 :: Generator g => g ()
dup2_x2 = i0 DUP2_X2
swap :: Generator g => g ()
swap    = i0 SWAP   
iadd :: Generator g => g ()
iadd    = i0 IADD   
ladd :: Generator g => g ()
ladd    = i0 LADD   
fadd :: Generator g => g ()
fadd    = i0 FADD   
dadd :: Generator g => g ()
dadd    = i0 DADD   
isub :: Generator g => g ()
isub    = i0 ISUB   
lsub :: Generator g => g ()
lsub    = i0 LSUB   
fsub :: Generator g => g ()
fsub    = i0 FSUB   
dsub :: Generator g => g ()
dsub    = i0 DSUB   
imul :: Generator g => g ()
imul    = i0 IMUL   
lmul :: Generator g => g ()
lmul    = i0 LMUL   
fmul :: Generator g => g ()
fmul    = i0 FMUL   
dmul :: Generator g => g ()
dmul    = i0 DMUL   
idiv :: Generator g => g ()
idiv    = i0 IDIV   
ldiv :: Generator g => g ()
ldiv    = i0 LDIV   
fdiv :: Generator g => g ()
fdiv    = i0 FDIV   
ddiv :: Generator g => g ()
ddiv    = i0 DDIV   
irem :: Generator g => g ()
irem    = i0 IREM   
lrem :: Generator g => g ()
lrem    = i0 LREM   
frem :: Generator g => g ()
frem    = i0 FREM   
drem :: Generator g => g ()
drem    = i0 DREM   
ineg :: Generator g => g ()
ineg    = i0 INEG   
lneg :: Generator g => g ()
lneg    = i0 LNEG   
fneg :: Generator g => g ()
fneg    = i0 FNEG   
dneg :: Generator g => g ()
dneg    = i0 DNEG   
ishl :: Generator g => g ()
ishl    = i0 ISHL   
lshl :: Generator g => g ()
lshl    = i0 LSHL   
ishr :: Generator g => g ()
ishr    = i0 ISHR   
lshr :: Generator g => g ()
lshr    = i0 LSHR   
iushr :: Generator g => g ()
iushr   = i0 IUSHR  
lushr :: Generator g => g ()
lushr   = i0 LUSHR  
iand :: Generator g => g ()
iand    = i0 IAND   
land :: Generator g => g ()
land    = i0 LAND   
ior :: Generator g => g ()
ior     = i0 IOR    
lor :: Generator g => g ()
lor     = i0 LOR    
ixor :: Generator g => g ()
ixor    = i0 IXOR   
lxor :: Generator g => g ()
lxor    = i0 LXOR   

iinc :: Generator g => Word8 -> Word8 -> g ()
iinc x y = i0 (IINC x y)

i2l :: Generator g => g ()
i2l  = i0 I2L 
i2f :: Generator g => g ()
i2f  = i0 I2F 
i2d :: Generator g => g ()
i2d  = i0 I2D 
l2i :: Generator g => g ()
l2i  = i0 L2I 
l2f :: Generator g => g ()
l2f  = i0 L2F 
l2d :: Generator g => g ()
l2d  = i0 L2D 
f2i :: Generator g => g ()
f2i  = i0 F2I 
f2l :: Generator g => g ()
f2l  = i0 F2L 
f2d :: Generator g => g ()
f2d  = i0 F2D 
d2i :: Generator g => g ()
d2i  = i0 D2I 
d2l :: Generator g => g ()
d2l  = i0 D2L 
d2f :: Generator g => g ()
d2f  = i0 D2F 
i2b :: Generator g => g ()
i2b  = i0 I2B 
i2c :: Generator g => g ()
i2c  = i0 I2C 
i2s :: Generator g => g ()
i2s  = i0 I2S 
lcmp :: Generator g => g ()
lcmp = i0 LCMP

-- | Wide instruction
wide :: Generator g => (Word8 -> Instruction) -> Constant Direct -> g ()
wide fn c = do
  ix <- addToPool c
  let ix0 = fromIntegral (ix `div` 0x100) :: Word8
      ix1 = fromIntegral (ix `mod` 0x100) :: Word8
  i0 (WIDE ix0 $ fn ix1)

new :: Generator g => B.ByteString -> g ()
new cls =
  i1 NEW (CClass cls)

newArray :: Generator g => ArrayType -> g ()
newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray :: Generator g => B.ByteString -> g ()
allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual :: Generator g => B.ByteString -> NameType Method -> g ()
invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic :: Generator g => B.ByteString -> NameType Method -> g ()
invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial :: Generator g => B.ByteString -> NameType Method -> g ()
invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

getStaticField :: Generator g => B.ByteString -> NameType Field -> g ()
getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

loadString :: Generator g => B.ByteString -> g ()
loadString str =
  i8 LDC1 (CString str)

allocArray :: Generator g => B.ByteString -> g ()
allocArray cls =
  i1 ANEWARRAY (CClass cls)

