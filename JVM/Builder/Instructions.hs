
module JVM.Builder.Instructions where

import JVM.ClassFile
import JVM.Assembler
import JVM.Builder.Monad

nop = i0 NOP
aconst_null = i0 ACONST_NULL
iconst_m1 = i0 ICONST_M1
iconst_0 = i0 ICONST_0
iconst_1 = i0 ICONST_1
iconst_2 = i0 ICONST_2
iconst_3 = i0 ICONST_3
iconst_4 = i0 ICONST_4
iconst_5 = i0 ICONST_5
lconst_0 = i0 LCONST_0
lconst_1 = i0 LCONST_1
fconst_0 = i0 FCONST_0
fconst_1 = i0 FCONST_1
fconst_2 = i0 FCONST_2
dconst_0 = i0 DCONST_0
dconst_1 = i0 DCONST_1

bipush x = i0 (BIPUSH x)
sipush x = i0 (SIPUSH x)
ldc1 x = i8 LDC1 x
ldc2 x = i1 LDC2 x
ldc2w x = i1 LDC2W x
iload x = i8 ILOAD x
lload x = i8 LLOAD x
fload x = i8 FLOAD x
dload x = i8 DLOAD x
aload x = i8 ALOAD x

iload_ x = i0 (ILOAD_ x)
lload_ x = i0 (LLOAD_ x)
fload_ x = i0 (FLOAD_ x)
dload_ x = i0 (DLOAD_ x)
aload_ x = i0 (ALOAD_ x)

iaload = i0 IALOAD
laload = i0 LALOAD
faload = i0 FALOAD
daload = i0 DALOAD
aaload = i0 AALOAD
caload = i0 CALOAD
saload = i0 SALOAD

istore x = i8 ISTORE x
lstore x = i8 LSTORE x
fstore x = i8 FSTORE x
dstore x = i8 DSTORE x
astore x = i8 ASTORE x

istore_ x = i0 (ISTORE x)
lstore_ x = i0 (LSTORE x)
fstore_ x = i0 (FSTORE x)
dstore_ x = i0 (DSTORE x)
astore_ x = i0 (ASTORE x)

iastore = i0 IASTORE
lastore = i0 LASTORE
fastore = i0 FASTORE
dastore = i0 DASTORE
aastore = i0 AASTORE
bastore = i0 BASTORE
castore = i0 CASTORE
sastore = i0 SASTORE

pop     = i0 POP    
pop2    = i0 POP2   
dup     = i0 DUP    
dup_x1  = i0 DUP_X1 
dup_x2  = i0 DUP_X2 
dup2    = i0 DUP2   
dup2_x1 = i0 DUP2_X1
dup2_x2 = i0 DUP2_X2
swap    = i0 SWAP   
iadd    = i0 IADD   
ladd    = i0 LADD   
fadd    = i0 FADD   
dadd    = i0 DADD   
isub    = i0 ISUB   
lsub    = i0 LSUB   
fsub    = i0 FSUB   
dsub    = i0 DSUB   
imul    = i0 IMUL   
lmul    = i0 LMUL   
fmul    = i0 FMUL   
dmul    = i0 DMUL   
idiv    = i0 IDIV   
ldiv    = i0 LDIV   
fdiv    = i0 FDIV   
ddiv    = i0 DDIV   
irem    = i0 IREM   
lrem    = i0 LREM   
frem    = i0 FREM   
drem    = i0 DREM   
ineg    = i0 INEG   
lneg    = i0 LNEG   
fneg    = i0 FNEG   
dneg    = i0 DNEG   
ishl    = i0 ISHL   
lshl    = i0 LSHL   
ishr    = i0 ISHR   
lshr    = i0 LSHR   
iushr   = i0 IUSHR  
lushr   = i0 LUSHR  
iand    = i0 IAND   
land    = i0 LAND   
ior     = i0 IOR    
lor     = i0 LOR    
ixor    = i0 IXOR   
lxor    = i0 LXOR   

iinc x y = i0 (IINC x y)

i2l  = i0 I2L 
i2f  = i0 I2F 
i2d  = i0 I2D 
l2i  = i0 L2I 
l2f  = i0 L2F 
l2d  = i0 L2D 
f2i  = i0 F2I 
f2l  = i0 F2L 
f2d  = i0 F2D 
d2i  = i0 D2I 
d2l  = i0 D2L 
d2f  = i0 D2F 
i2b  = i0 I2B 
i2c  = i0 I2C 
i2s  = i0 I2S 
lcmp = i0 LCMP

new cls =
  i1 NEW (CClass cls)

newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

loadString str =
  i8 LDC1 (CString str)

allocArray cls =
  i1 ANEWARRAY (CClass cls)

