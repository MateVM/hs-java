-- | This module exports shortcuts for some of JVM instructions (which are defined in JVM.Assembler).
-- These functions get Constants, put them into constants pool and generate instruction using index
-- of constant in the pool.
module JVM.Builder.Instructions where

import Data.Word
import qualified Data.ByteString.Lazy as B

import JVM.ClassFile
import JVM.Assembler
import JVM.Builder.Monad

nop ::  Generate ()
nop = i0 NOP
aconst_null ::  Generate ()
aconst_null = i0 ACONST_NULL
iconst_m1 ::  Generate ()
iconst_m1 = i0 ICONST_M1
iconst_0 ::  Generate ()
iconst_0 = i0 ICONST_0
iconst_1 ::  Generate ()
iconst_1 = i0 ICONST_1
iconst_2 ::  Generate ()
iconst_2 = i0 ICONST_2
iconst_3 ::  Generate ()
iconst_3 = i0 ICONST_3
iconst_4 ::  Generate ()
iconst_4 = i0 ICONST_4
iconst_5 ::  Generate ()
iconst_5 = i0 ICONST_5
lconst_0 ::  Generate ()
lconst_0 = i0 LCONST_0
lconst_1 ::  Generate ()
lconst_1 = i0 LCONST_1
fconst_0 ::  Generate ()
fconst_0 = i0 FCONST_0
fconst_1 ::  Generate ()
fconst_1 = i0 FCONST_1
fconst_2 ::  Generate ()
fconst_2 = i0 FCONST_2
dconst_0 ::  Generate ()
dconst_0 = i0 DCONST_0
dconst_1 ::  Generate ()
dconst_1 = i0 DCONST_1

bipush ::  Word8 -> Generate ()
bipush x = i0 (BIPUSH x)
sipush ::  Word16 -> Generate ()
sipush x = i0 (SIPUSH x)

ldc1 ::  Constant Direct -> Generate ()
ldc1 x = i8 LDC1 x
ldc2 ::  Constant Direct -> Generate ()
ldc2 x = i1 LDC2 x
ldc2w ::  Constant Direct -> Generate ()
ldc2w x = i1 LDC2W x
iload ::  Constant Direct -> Generate ()
iload x = i8 ILOAD x
lload ::  Constant Direct -> Generate ()
lload x = i8 LLOAD x
fload ::  Constant Direct -> Generate ()
fload x = i8 FLOAD x
dload ::  Constant Direct -> Generate ()
dload x = i8 DLOAD x
aload ::  Constant Direct -> Generate ()
aload x = i8 ALOAD x

iload_ ::  IMM -> Generate ()
iload_ x = i0 (ILOAD_ x)
lload_ ::  IMM -> Generate ()
lload_ x = i0 (LLOAD_ x)
fload_ ::  IMM -> Generate ()
fload_ x = i0 (FLOAD_ x)
dload_ ::  IMM -> Generate ()
dload_ x = i0 (DLOAD_ x)
aload_ ::  IMM -> Generate ()
aload_ x = i0 (ALOAD_ x)

iaload ::  Generate ()
iaload = i0 IALOAD
laload ::  Generate ()
laload = i0 LALOAD
faload ::  Generate ()
faload = i0 FALOAD
daload ::  Generate ()
daload = i0 DALOAD
aaload ::  Generate ()
aaload = i0 AALOAD
caload ::  Generate ()
caload = i0 CALOAD
saload ::  Generate ()
saload = i0 SALOAD

istore ::  Constant Direct -> Generate ()
istore x = i8 ISTORE x
lstore ::  Constant Direct -> Generate ()
lstore x = i8 LSTORE x
fstore ::  Constant Direct -> Generate ()
fstore x = i8 FSTORE x
dstore ::  Constant Direct -> Generate ()
dstore x = i8 DSTORE x
astore ::  Constant Direct -> Generate ()
astore x = i8 ASTORE x

istore_ ::  Word8 -> Generate ()
istore_ x = i0 (ISTORE x)
lstore_ ::  Word8 -> Generate ()
lstore_ x = i0 (LSTORE x)
fstore_ ::  Word8 -> Generate ()
fstore_ x = i0 (FSTORE x)
dstore_ ::  Word8 -> Generate ()
dstore_ x = i0 (DSTORE x)
astore_ ::  Word8 -> Generate ()
astore_ x = i0 (ASTORE x)

iastore ::  Generate ()
iastore = i0 IASTORE
lastore ::  Generate ()
lastore = i0 LASTORE
fastore ::  Generate ()
fastore = i0 FASTORE
dastore ::  Generate ()
dastore = i0 DASTORE
aastore ::  Generate ()
aastore = i0 AASTORE
bastore ::  Generate ()
bastore = i0 BASTORE
castore ::  Generate ()
castore = i0 CASTORE
sastore ::  Generate ()
sastore = i0 SASTORE

pop ::  Generate ()
pop     = i0 POP    
pop2 ::  Generate ()
pop2    = i0 POP2   
dup ::  Generate ()
dup     = i0 DUP    
dup_x1 ::  Generate ()
dup_x1  = i0 DUP_X1 
dup_x2 ::  Generate ()
dup_x2  = i0 DUP_X2 
dup2 ::  Generate ()
dup2    = i0 DUP2   
dup2_x1 ::  Generate ()
dup2_x1 = i0 DUP2_X1
dup2_x2 ::  Generate ()
dup2_x2 = i0 DUP2_X2
swap ::  Generate ()
swap    = i0 SWAP   
iadd ::  Generate ()
iadd    = i0 IADD   
ladd ::  Generate ()
ladd    = i0 LADD   
fadd ::  Generate ()
fadd    = i0 FADD   
dadd ::  Generate ()
dadd    = i0 DADD   
isub ::  Generate ()
isub    = i0 ISUB   
lsub ::  Generate ()
lsub    = i0 LSUB   
fsub ::  Generate ()
fsub    = i0 FSUB   
dsub ::  Generate ()
dsub    = i0 DSUB   
imul ::  Generate ()
imul    = i0 IMUL   
lmul ::  Generate ()
lmul    = i0 LMUL   
fmul ::  Generate ()
fmul    = i0 FMUL   
dmul ::  Generate ()
dmul    = i0 DMUL   
idiv ::  Generate ()
idiv    = i0 IDIV   
ldiv ::  Generate ()
ldiv    = i0 LDIV   
fdiv ::  Generate ()
fdiv    = i0 FDIV   
ddiv ::  Generate ()
ddiv    = i0 DDIV   
irem ::  Generate ()
irem    = i0 IREM   
lrem ::  Generate ()
lrem    = i0 LREM   
frem ::  Generate ()
frem    = i0 FREM   
drem ::  Generate ()
drem    = i0 DREM   
ineg ::  Generate ()
ineg    = i0 INEG   
lneg ::  Generate ()
lneg    = i0 LNEG   
fneg ::  Generate ()
fneg    = i0 FNEG   
dneg ::  Generate ()
dneg    = i0 DNEG   
ishl ::  Generate ()
ishl    = i0 ISHL   
lshl ::  Generate ()
lshl    = i0 LSHL   
ishr ::  Generate ()
ishr    = i0 ISHR   
lshr ::  Generate ()
lshr    = i0 LSHR   
iushr ::  Generate ()
iushr   = i0 IUSHR  
lushr ::  Generate ()
lushr   = i0 LUSHR  
iand ::  Generate ()
iand    = i0 IAND   
land ::  Generate ()
land    = i0 LAND   
ior ::  Generate ()
ior     = i0 IOR    
lor ::  Generate ()
lor     = i0 LOR    
ixor ::  Generate ()
ixor    = i0 IXOR   
lxor ::  Generate ()
lxor    = i0 LXOR   

iinc ::  Word8 -> Word8 -> Generate ()
iinc x y = i0 (IINC x y)

i2l ::  Generate ()
i2l  = i0 I2L 
i2f ::  Generate ()
i2f  = i0 I2F 
i2d ::  Generate ()
i2d  = i0 I2D 
l2i ::  Generate ()
l2i  = i0 L2I 
l2f ::  Generate ()
l2f  = i0 L2F 
l2d ::  Generate ()
l2d  = i0 L2D 
f2i ::  Generate ()
f2i  = i0 F2I 
f2l ::  Generate ()
f2l  = i0 F2L 
f2d ::  Generate ()
f2d  = i0 F2D 
d2i ::  Generate ()
d2i  = i0 D2I 
d2l ::  Generate ()
d2l  = i0 D2L 
d2f ::  Generate ()
d2f  = i0 D2F 
i2b ::  Generate ()
i2b  = i0 I2B 
i2c ::  Generate ()
i2c  = i0 I2C 
i2s ::  Generate ()
i2s  = i0 I2S 
lcmp ::  Generate ()
lcmp = i0 LCMP

-- | Wide instruction
wide :: (Word8 -> Instruction) -> Constant Direct -> Generate ()
wide fn c = do
  ix <- addToPool c
  let ix0 = fromIntegral (ix `div` 0x100) :: Word8
      ix1 = fromIntegral (ix `mod` 0x100) :: Word8
  i0 (WIDE ix0 $ fn ix1)

new ::  B.ByteString -> Generate ()
new cls =
  i1 NEW (CClass cls)

newArray ::  ArrayType -> Generate ()
newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray ::  B.ByteString -> Generate ()
allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual ::  B.ByteString -> NameType Method -> Generate ()
invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic ::  B.ByteString -> NameType Method -> Generate ()
invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial ::  B.ByteString -> NameType Method -> Generate ()
invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

getStaticField ::  B.ByteString -> NameType Field -> Generate ()
getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

loadString ::  B.ByteString -> Generate ()
loadString str =
  i8 LDC1 (CString str)

allocArray ::  B.ByteString -> Generate ()
allocArray cls =
  i1 ANEWARRAY (CClass cls)

