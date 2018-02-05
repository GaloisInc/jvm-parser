{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Language.JVM.Common
Copyright   : Galois, Inc. 2012-2014
License     : BSD3
Maintainer  : atomb@galois.com
Stability   : stable
Portability : non-portable

Basic datatypes and utilities for the JVM parser.
-}

module Language.JVM.Common
  ( -- * Miscellaneous
    slashesToDots
  , dotsToSlashes
    -- * Class names
  , ClassName
  , mkClassName
  , unClassName
    -- * Types
  , Type(..)
  , stringTy
  , intArrayTy
  , byteArrayTy
  , charArrayTy
  , isIValue
  , isPrimitiveType
  , stackWidth
  , isFloatType
  , isRefType
  , ppType
    -- * FieldId
  , FieldId(..)
  , ppFldId
    -- * MethodKey
  , MethodKey(..)
  , ppMethodKey
    -- * Instructions
  , ConstantPoolValue(..)
  , LocalVariableIndex
  , PC
  , Instruction(..)
  , ppInstruction
  , InstructionStream
  , canThrowException
  , isArrayLoad
  , isReturn
  , breaksControlFlow
  , nextPcPrim
  , safeNextPcPrim
    -- * ExceptionTable
  , ExceptionTableEntry(..)
  , ExceptionTable
  ) where

import Data.Array
import Data.Int
import Data.String (IsString(..))
import Data.Word
import Text.PrettyPrint

-- | Replace '/' characters with '.' characters
slashesToDots :: String -> String
slashesToDots = map (\c -> if c == '/' then '.' else c)

-- | Replace '.' characters with '/' characters
dotsToSlashes :: String -> String
dotsToSlashes = map (\c -> if c == '.' then '/' else c)

-- | Name of a Java class, with names of packages separated by slashes '/'.
newtype ClassName = ClassName String
  deriving (Eq, Ord, Show)

instance IsString ClassName where
  fromString s = ClassName s

-- | Make a class name from a string with packages separated by slashes.
mkClassName :: String -> ClassName
mkClassName s = ClassName s

-- | Print class name with names of packages separated by slashes.
unClassName :: ClassName -> String
unClassName (ClassName st) = st

-- | JVM Type
data Type
  = ArrayType Type
  | BooleanType
  | ByteType
  | CharType
  | ClassType ClassName
  | DoubleType
  | FloatType
  | IntType
  | LongType
  | ShortType
  deriving (Eq, Ord)

stringTy :: Type
stringTy = ClassType "java/lang/String"

intArrayTy :: Type
intArrayTy = ArrayType IntType

byteArrayTy :: Type
byteArrayTy = ArrayType ByteType

charArrayTy :: Type
charArrayTy = ArrayType CharType

-- | Returns true if type is an integer value.
isIValue :: Type -> Bool
isIValue BooleanType = True
isIValue ByteType    = True
isIValue CharType    = True
isIValue IntType     = True
isIValue ShortType   = True
isIValue _           = False

-- | Returns true if Java type is a primitive type.  Primitive types are
-- the Boolean type or numeric types.
isPrimitiveType :: Type -> Bool
isPrimitiveType (ArrayType _) = False
isPrimitiveType BooleanType   = True
isPrimitiveType ByteType      = True
isPrimitiveType CharType      = True
isPrimitiveType (ClassType _) = False
isPrimitiveType DoubleType    = True
isPrimitiveType FloatType     = True
isPrimitiveType IntType       = True
isPrimitiveType LongType      = True
isPrimitiveType ShortType     = True

-- | Returns number of bits that a Java type is expected to take on the stack.
-- Type should be a primitive type.
stackWidth :: Type -> Int
stackWidth BooleanType = 32
stackWidth ByteType    = 32
stackWidth CharType    = 32
stackWidth DoubleType  = 64
stackWidth FloatType   = 32
stackWidth IntType     = 32
stackWidth LongType    = 64
stackWidth ShortType   = 32
stackWidth _ = error "internal: illegal type"

-- | Returns true if Java type denotes a floating point.
isFloatType :: Type -> Bool
isFloatType FloatType = True
isFloatType DoubleType = True
isFloatType _ = False

-- | Returns true if Java type denotes a reference (i.e. array or class).
isRefType :: Type -> Bool
isRefType (ArrayType _) = True
isRefType (ClassType _) = True
isRefType _ = False

-- | Unique identifier of field
data FieldId = FieldId {
    fieldIdClass :: !ClassName -- ^ Class name
  , fieldIdName  :: !String -- ^ Field name
  , fieldIdType  :: !Type   -- ^ Field type
  } deriving (Eq, Ord, Show)

ppFldId :: FieldId -> String
ppFldId fldId = dotsToSlashes (unClassName (fieldIdClass fldId)) ++ "." ++ fieldIdName fldId

-- MethodKey {{{1
-- | A unique identifier for looking up a method in a class.
data MethodKey = MethodKey {
    methodKeyName :: String
  , methodKeyParameterTypes :: [Type]
  , methodKeyReturnType :: Maybe Type
  } deriving (Eq, Ord, Show)

ppMethodKey :: MethodKey -> Doc
ppMethodKey (MethodKey name params ret) =
       text name
    <> (parens . commas . map ppType) params
    <> maybe "void" ppType ret
  where commas = sep . punctuate comma

-- | A value stored in the constant pool.
data ConstantPoolValue
  = Long Int64
  | Float Float
  | Double Double
  | Integer Int32
  | String String
  | ClassRef ClassName
  deriving (Eq, Show)

-- | A local variable index.
type LocalVariableIndex = Word16

-- | A program counter value.
type PC = Word16

-- | A JVM instruction.
data Instruction
  = Aaload
  | Aastore
  | Aconst_null
  | Aload LocalVariableIndex
  -- Anewarray replaced by generalized Newarray
  | Areturn
  | Arraylength
  | Astore LocalVariableIndex
  | Athrow
  | Baload
  | Bastore
  -- Bipush replaced with Ldc
  | Caload
  | Castore
  | Checkcast Type
  | D2f
  | D2i
  | D2l
  | Dadd
  | Daload
  | Dastore
  | Dcmpg
  | Dcmpl
  -- Dconst_x has been replaced by Ldc
  | Ddiv
  | Dload LocalVariableIndex
  | Dmul
  | Dneg
  | Drem
  | Dreturn
  | Dstore LocalVariableIndex
  | Dsub
  | Dup
  | Dup_x1
  | Dup_x2
  | Dup2
  | Dup2_x1
  | Dup2_x2
  | F2d
  | F2i
  | F2l
  | Fadd
  | Faload
  | Fastore
  | Fcmpg
  | Fcmpl
  -- Fconst_x has been replaced by Ldc
  | Fdiv
  | Fload LocalVariableIndex
  | Fmul
  | Fneg
  | Frem
  | Freturn
  | Fstore LocalVariableIndex
  | Fsub
  -- | getfield instruction
  | Getfield FieldId
  | Getstatic FieldId
  | Goto PC
  -- Goto_w has been replaced with Goto
  | I2b
  | I2c
  | I2d
  | I2f
  | I2l
  | I2s
  | Iadd
  | Iaload
  | Iand
  | Iastore
  -- Iconst_x replaced with sipush
  | Idiv
  | If_acmpeq PC
  | If_acmpne PC
  | If_icmpeq PC
  | If_icmpne PC
  | If_icmplt PC
  | If_icmpge PC
  | If_icmpgt PC
  | If_icmple PC
  | Ifeq PC
  | Ifne PC
  | Iflt PC
  | Ifge PC
  | Ifgt PC
  | Ifle PC
  | Ifnonnull PC
  | Ifnull PC
  | Iinc LocalVariableIndex Int16
  | Iload LocalVariableIndex
  | Imul
  | Ineg
  | Instanceof Type
  -- | Since we don't yet attempt to resolve @invokedynamic@ targets,
  -- just store the constant pool index for the call site specifier
  | Invokedynamic   Word16
  | Invokeinterface ClassName MethodKey
  | Invokespecial   Type      MethodKey
  | Invokestatic    ClassName MethodKey
  | Invokevirtual   Type      MethodKey
  | Ior
  | Irem
  | Ireturn
  | Ishl
  | Ishr
  | Istore LocalVariableIndex
  | Isub
  | Iushr
  | Ixor
  | Jsr PC
  | L2d
  | L2f
  | L2i
  | Ladd
  | Laload
  | Land
  | Lastore
  | Lcmp
  -- Lconst_x has been replaced by generalized Ldc
  -- Ldc, Ldc_w and Ldc2_w have been merged into single generalized Ldc
  | Ldc ConstantPoolValue
  | Ldiv
  | Lload LocalVariableIndex
  | Lmul
  | Lneg
  | Lookupswitch PC {-default -} [(Int32,PC)] {- (key, target) -}
  | Lor
  | Lrem
  | Lreturn
  | Lshl
  | Lshr
  | Lstore LocalVariableIndex
  | Lsub
  | Lushr
  | Lxor
  | Monitorenter
  | Monitorexit
  | Multianewarray Type Word8
  | New ClassName
  -- The type is the type of the array.
  | Newarray Type
  | Nop
  | Pop
  | Pop2
  | Putfield  FieldId
  | Putstatic FieldId
  | Ret LocalVariableIndex
  | Return
  | Saload
  | Sastore
  -- Sipush has been replced by ldc
  | Swap
  | Tableswitch PC Int32 Int32 [PC]
  deriving (Eq,Show)

-- TODO: improve this
ppInstruction :: Instruction -> Doc
ppInstruction = text . show

-- | An entry in the exception table for a method
data ExceptionTableEntry = ExceptionTableEntry {
  -- | The starting program counter value where the exception handler applies
    startPc :: PC
  -- | The ending program counter value where the exception handler applies.
  , endPc :: PC
  -- | The program counter value to jump to when an exception is caught.
  , handlerPc :: PC
  -- | The type of exception that should be caught or 'Nothing' if all types of
  -- exceptions should be caught.
  , catchType :: Maybe Type
  } deriving (Eq,Show)

type ExceptionTable = [ExceptionTableEntry]

type InstructionStream = Array PC (Maybe Instruction)

--------------------------------------------------------------------------------
-- Utility functions

canThrowException :: Instruction -> Bool
canThrowException Arraylength{}     = True
canThrowException Checkcast{}       = True
canThrowException Getfield{}        = True
canThrowException Getstatic{}       = True
canThrowException Idiv{}            = True
canThrowException Invokeinterface{} = True
canThrowException Invokespecial{}   = True
canThrowException Invokestatic{}    = True
canThrowException Invokevirtual{}   = True
canThrowException Irem{}            = True
canThrowException Ldiv{}            = True
canThrowException Lrem{}            = True
canThrowException Monitorenter{}    = True
canThrowException Monitorexit{}     = True
canThrowException Multianewarray{}  = True
canThrowException Newarray{}        = True
canThrowException New{}             = True
canThrowException Putfield{}        = True
canThrowException Putstatic{}       = True
canThrowException Athrow{}          = True
canThrowException inst              = isArrayLoad inst || isReturn inst

isArrayLoad :: Instruction -> Bool
isArrayLoad Aaload{}  = True
isArrayLoad Aastore{} = True
isArrayLoad Baload{}  = True
isArrayLoad Bastore{} = True
isArrayLoad Caload{}  = True
isArrayLoad Castore{} = True
isArrayLoad Daload{}  = True
isArrayLoad Dastore{} = True
isArrayLoad Faload{}  = True
isArrayLoad Fastore{} = True
isArrayLoad Iaload{}  = True
isArrayLoad Iastore{} = True
isArrayLoad Laload{}  = True
isArrayLoad Lastore{} = True
isArrayLoad Saload{}  = True
isArrayLoad Sastore{} = True
isArrayLoad _         = False

isReturn :: Instruction -> Bool
isReturn Areturn{} = True
isReturn Dreturn{} = True
isReturn Freturn{} = True
isReturn Ireturn{} = True
isReturn Lreturn{} = True
isReturn Return{}  = True
isReturn _         = False

breaksControlFlow :: Instruction -> Bool
breaksControlFlow Jsr{}    = True
breaksControlFlow Ret{}    = True
breaksControlFlow Goto{}   = True
breaksControlFlow Athrow{} = True
breaksControlFlow inst     = isReturn inst

nextPcPrim :: InstructionStream -> PC -> PC
nextPcPrim istrm pc = findNext istrm (pc + 1)
  where findNext is i =
          case (is ! i) of
            Just _  -> i
            Nothing -> findNext is (i+1)

safeNextPcPrim :: InstructionStream -> PC -> Maybe PC
safeNextPcPrim istrm pc | pc <= snd (bounds istrm) = Just $ nextPcPrim istrm pc
                        | otherwise                = Nothing

--------------------------------------------------------------------------------
-- Instances

instance Show Type where
  show ByteType       = "byte"
  show CharType       = "char"
  show DoubleType     = "double"
  show FloatType      = "float"
  show IntType        = "int"
  show LongType       = "long"
  show (ClassType cn) = slashesToDots (unClassName cn)
  show ShortType      = "short"
  show BooleanType    = "boolean"
  show (ArrayType tp) = (show tp) ++ "[]"

ppType :: Type -> Doc
ppType = text . show
